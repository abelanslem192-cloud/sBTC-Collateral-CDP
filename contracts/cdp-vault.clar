(use-trait sip010-token .sip-010-trait.sip-010-trait)
(use-trait stable-token-trait .sip-010-trait.sip-010-trait)
(use-trait flash-loan-trait .flash-loan-trait.flash-loan-trait)
(use-trait oracle-trait .oracle-trait.oracle-trait)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-collateral (err u101))
(define-constant err-vault-exists (err u102))
(define-constant err-vault-not-found (err u103))
(define-constant err-under-collateralized (err u104))
(define-constant err-repayment-too-high (err u105))
(define-constant err-liquidation-not-allowed (err u106))
(define-constant err-transfer-failed (err u107))
(define-constant err-collateral-not-supported (err u108))
(define-constant err-debt-ceiling-reached (err u109))
(define-constant err-oracle-error (err u110))
(define-constant err-unauthorized (err u111))
(define-constant err-invalid-fee-split (err u112))

(define-constant liquidation-penalty u10) ;; 10% total penalty
(define-constant oracle-decimals u8)
(define-constant flash-mint-fee u10)
(define-constant scale-factor u100000000)
(define-constant target-ratio-after-liquidation u175)
(define-constant critical-ratio u100)

;; Reserve for protocol fees (collected from liquidation penalties)
(define-map stability-reserve principal uint)

;; Fee Split Configuration
;; Default: 80% to Keeper, 20% to Protocol
(define-data-var keeper-reward-ratio uint u80) 
(define-data-var protocol-fee-ratio uint u20)

(define-map supported-collateral
    principal
    {
        name: (string-ascii 32),
        liquidation-ratio: uint,
        debt-ceiling: uint,
        interest-rate: uint, ;; Stability Fee per year (e.g. 5% = 0.05 * 10^8)
        total-debt: uint,
        active: bool,
        ;; Feature 6: Index-Based Tracking
        current-index: uint, ;; Starts at 1.0 (u100000000)
        last-updated-block: uint
    }
)

;; Single Oracle Registry for all assets
(define-data-var oracle-registry-contract principal tx-sender)

(define-map vaults
    { owner: principal, collateral-token: principal }
    {
        collateral: uint,
        normalized-debt: uint, ;; Feature 6: Debt / Index
        ;; last-accrued-block removed, tracked globally
    }
)

(define-map vault-delegates
    { owner: principal, delegate: principal }
    bool
)

;; Feature 6: Accrue Global Interest
;; Should be called before any interaction modifying debt/collateral
(define-public (accrue-interest (token principal))
    (let (
            (type-info (unwrap! (map-get? supported-collateral token) err-collateral-not-supported))
            (current-index (get current-index type-info))
            (last-block (get last-updated-block type-info))
            (delta-blocks (- burn-block-height last-block))
            (rate (get interest-rate type-info))
        )
        (if (> delta-blocks u0)
            (let (
                    ;; New Index = Old Index * (1 + rate * time)
                    ;; Interest Factor = rate * delta-blocks / scale-factor (assuming rate is annualized-ish/per-block scaled)
                    ;; For simplicity in MVP: rate is "rate per block" scaled by 10^8.
                    ;; Actual math: (index * (scale + rate * delta)) / scale
                    (interest-factor (* rate delta-blocks))
                    (new-index (/ (* current-index (+ scale-factor interest-factor)) scale-factor))
                )
                (ok (map-set supported-collateral token (merge type-info {
                    current-index: new-index,
                    last-updated-block: burn-block-height
                })))
            )
            (ok true) ;; No time passed, no update needed
        )
    )
)

(define-private (is-authorized (vault-owner principal) (caller principal))
    (or 
        (is-eq vault-owner caller)
        (default-to false (map-get? vault-delegates { owner: vault-owner, delegate: caller }))
    )
)

(define-read-only (get-collateral-type (token principal))
    (map-get? supported-collateral token)
)

(define-read-only (get-vault (user principal) (token principal))
    (default-to {
        collateral: u0,
        normalized-debt: u0
    }
        (map-get? vaults { owner: user, collateral-token: token })
    )
)

;; Helper to fetch latest price from registry
(define-public (get-price-from-oracle (token principal) (oracle <oracle-trait>))
    (begin
        ;; Ensure passed oracle trait matches registry
        (asserts! (is-eq (contract-of oracle) (var-get oracle-registry-contract)) err-oracle-error)
        (contract-call? oracle get-price token)
    )
)

(define-read-only (calculate-collateral-value (collateral-amount uint) (price uint))
    (/ (* collateral-amount price) u100000000)
)

(define-read-only (get-total-debt (vault-debt-normalized uint) (current-index uint))
    (/ (* vault-debt-normalized current-index) scale-factor)
)

(define-public (calculate-current-ratio (user principal) (token principal) (oracle <oracle-trait>))
    (let (
            (vault (get-vault user token))
            (type-info (unwrap! (get-collateral-type token) err-collateral-not-supported))
            (price (unwrap! (get-price-from-oracle token oracle) err-oracle-error))
            (collateral-val (calculate-collateral-value (get collateral vault) price))
            
            ;; Calculate current aggregated debt (simulate accrual)
            (current-index (get current-index type-info))
            (delta-blocks (- burn-block-height (get last-updated-block type-info)))
            (rate (get interest-rate type-info))
            (simulated-index (if (> delta-blocks u0)
                 (/ (* current-index (+ scale-factor (* rate delta-blocks))) scale-factor)
                 current-index
            ))
            (total-debt (get-total-debt (get normalized-debt vault) simulated-index))
        )
        (if (is-eq total-debt u0)
            (ok u99999999)
            (ok (/ (* collateral-val u100) total-debt))
        )
    )
)

;; Admin Functions

(define-public (set-oracle-registry (registry principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set oracle-registry-contract registry))
    )
)

(define-public (set-fee-split (keeper-ratio uint) (protocol-ratio uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-eq (+ keeper-ratio protocol-ratio) u100) (err u112)) ;; Ratio must sum to 100
        (var-set keeper-reward-ratio keeper-ratio)
        (var-set protocol-fee-ratio protocol-ratio)
        (ok true)
    )
)

(define-public (withdraw-reserve (amount uint) (collateral-token <sip010-token>) (recipient principal))
    (let (
            (token-principal (contract-of collateral-token))
            (current-reserve (default-to u0 (map-get? stability-reserve token-principal)))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (>= current-reserve amount) err-insufficient-collateral)
        
        (try! (as-contract (contract-call? collateral-token transfer amount tx-sender recipient none)))
        (map-set stability-reserve token-principal (- current-reserve amount))
        (ok amount)
    )
)

(define-public (add-collateral-type
        (token principal)
        (name (string-ascii 32))
        (liquidation-ratio uint)
        (debt-ceiling uint)
        (interest-rate uint)
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set supported-collateral token {
            name: name,
            liquidation-ratio: liquidation-ratio,
            debt-ceiling: debt-ceiling,
            interest-rate: interest-rate,
            total-debt: u0,
            active: true,
            current-index: scale-factor, ;; Init at 1.0
            last-updated-block: burn-block-height
        }))
    )
)

(define-public (set-stability-fee (token principal) (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        ;; Accrue interest first to lock in old rate for past period
        (try! (accrue-interest token))
        (let (
            (type-info (unwrap! (map-get? supported-collateral token) err-collateral-not-supported))
        )
            (ok (map-set supported-collateral token (merge type-info { interest-rate: new-rate })))
        )
    )
)

;; Delegation Management

(define-public (add-delegate (delegate principal))
    (ok (map-set vault-delegates { owner: tx-sender, delegate: delegate } true))
)

(define-public (remove-delegate (delegate principal))
    (ok (map-delete vault-delegates { owner: tx-sender, delegate: delegate }))
)

;; User Functions

(define-public (deposit-collateral (amount uint) (collateral-token <sip010-token>) (vault-owner principal))
    (let (
            (token-principal (contract-of collateral-token))
        )
        (try! (accrue-interest token-principal))
        (let (
            (type-info (unwrap! (map-get? supported-collateral token-principal) err-collateral-not-supported))
            (vault (get-vault vault-owner token-principal))
            (current-collateral (get collateral vault))
            (new-collateral (+ current-collateral amount))
        )
            (asserts! (is-authorized vault-owner tx-sender) err-unauthorized)
            (asserts! (get active type-info) err-collateral-not-supported)
            
            ;; Transfer from caller (delegate or owner) to contract
            (try! (contract-call? collateral-token transfer amount tx-sender (as-contract tx-sender) none))
            
            (map-set vaults { owner: vault-owner, collateral-token: token-principal } {
                collateral: new-collateral,
                normalized-debt: (get normalized-debt vault)
            })
            (ok new-collateral)
        )
    )
)

(define-public (borrow (amount uint) (collateral-token <sip010-token>) (oracle <oracle-trait>) (vault-owner principal))
    (let (
            (token-principal (contract-of collateral-token))
        )
        (try! (accrue-interest token-principal))
        (let (
                (type-info (unwrap! (map-get? supported-collateral token-principal) err-collateral-not-supported))
                (vault (get-vault vault-owner token-principal))
                (price (unwrap! (get-price-from-oracle token-principal oracle) err-oracle-error))
                (collateral-val (calculate-collateral-value (get collateral vault) price))
                
                (normalized-amount (/ (* amount scale-factor) (get current-index type-info)))
                (new-normalized-debt (+ (get normalized-debt vault) normalized-amount))
                (new-actual-debt (get-total-debt new-normalized-debt (get current-index type-info)))
            )
            (asserts! (is-authorized vault-owner tx-sender) err-unauthorized)
            (asserts! (get active type-info) err-collateral-not-supported)
            (asserts! (>= (/ (* collateral-val u100) new-actual-debt) (get liquidation-ratio type-info)) err-under-collateralized)
            (asserts! (<= (+ (get total-debt type-info) amount) (get debt-ceiling type-info)) err-debt-ceiling-reached)

            ;; Mint stablecoin to VAULT OWNER, never delegate
            (try! (contract-call? .stable-token mint-for-vault amount vault-owner))

            (map-set vaults { owner: vault-owner, collateral-token: token-principal } {
                collateral: (get collateral vault),
                normalized-debt: new-normalized-debt
            })
            
            (map-set supported-collateral token-principal (merge type-info { total-debt: (+ (get total-debt type-info) amount) }))
            
            (ok new-actual-debt)
        )
    )
)

(define-public (repay (amount uint) (collateral-token <sip010-token>) (vault-owner principal))
    (let (
            (token-principal (contract-of collateral-token))
        )
        (try! (accrue-interest token-principal))
        (let (
                (type-info (unwrap! (map-get? supported-collateral token-principal) err-collateral-not-supported))
                (vault (get-vault vault-owner token-principal))
                (current-index (get current-index type-info))
                (current-actual-debt (get-total-debt (get normalized-debt vault) current-index))
            )
            ;; Anyone can repay? Or only authorized? Sticking to authorized for now as per plan logic
            (asserts! (is-authorized vault-owner tx-sender) err-unauthorized)
            (asserts! (<= amount current-actual-debt) err-repayment-too-high)

            (let (
                (repay-normalized (/ (* amount scale-factor) current-index))
                (new-normalized-debt (- (get normalized-debt vault) repay-normalized))
            )
                ;; Burn stablecoin from caller (delegate or owner)
                (try! (contract-call? .stable-token burn-for-vault amount tx-sender))

                (map-set vaults { owner: vault-owner, collateral-token: token-principal } {
                    collateral: (get collateral vault),
                    normalized-debt: new-normalized-debt
                })
                
                (map-set supported-collateral token-principal (merge type-info { total-debt: (- (get total-debt type-info) amount) }))
                
                (ok (- current-actual-debt amount))
            )
        )
    )
)

(define-public (withdraw-collateral (amount uint) (collateral-token <sip010-token>) (oracle <oracle-trait>) (vault-owner principal))
    (let (
            (token-principal (contract-of collateral-token))
        )
        (try! (accrue-interest token-principal))
        (let (
                (type-info (unwrap! (map-get? supported-collateral token-principal) err-collateral-not-supported))
                (vault (get-vault vault-owner token-principal))
                (price (unwrap! (get-price-from-oracle token-principal oracle) err-oracle-error))
                (current-collateral (get collateral vault))
                (new-collateral (- current-collateral amount))
                (current-actual-debt (get-total-debt (get normalized-debt vault) (get current-index type-info)))
                (collateral-val (calculate-collateral-value new-collateral price))
            )
            (asserts! (is-authorized vault-owner tx-sender) err-unauthorized)
            (asserts! (>= current-collateral amount) err-insufficient-collateral)
            (asserts!
                (or (is-eq current-actual-debt u0) (>= (/ (* collateral-val u100) current-actual-debt) (get liquidation-ratio type-info)))
                err-under-collateralized
            )

            ;; Transfer collateral to VAULT OWNER, never delegate
            (try! (as-contract (contract-call? collateral-token transfer amount tx-sender vault-owner none)))

            (map-set vaults { owner: vault-owner, collateral-token: token-principal } {
                collateral: new-collateral,
                normalized-debt: (get normalized-debt vault)
            })
            (ok new-collateral)
        )
    )
)

(define-private (liquidate-full (target principal) (collateral-token <sip010-token>) (oracle <oracle-trait>))
    (let (
            (token-principal (contract-of collateral-token))
        )
        ;; Note: private function, caller (partial or public liquidate) should have accrued interest? 
        ;; No, safest to accrue here too or ensure caller did.
        ;; Since liquidate-full is called by private/public partial, let's assume it's clean if we accrue inside.
        (try! (accrue-interest token-principal))
        (let (
                (liquidator tx-sender)
                (type-info (unwrap! (map-get? supported-collateral token-principal) err-collateral-not-supported))
                (vault (get-vault target token-principal))
                (price (unwrap! (get-price-from-oracle token-principal oracle) err-oracle-error))
                (collateral (get collateral vault))
                (debt (get-total-debt (get normalized-debt vault) (get current-index type-info)))
                (collateral-val (calculate-collateral-value collateral price))
                (ratio (if (is-eq debt u0) u99999999 (/ (* collateral-val u100) debt)))
            )
            (asserts! (< ratio (get liquidation-ratio type-info)) err-liquidation-not-allowed)

            (let (
                    (debt-value-in-collateral (/ (* debt u100000000) price))
                    (total-penalty (/ (* debt-value-in-collateral liquidation-penalty) u100))
                    (total-reward (+ debt-value-in-collateral total-penalty))
                    
                    ;; Calculate Split
                    (keeper-share (/ (* total-penalty (var-get keeper-reward-ratio)) u100))
                    (protocol-share (- total-penalty keeper-share))
                    
                    ;; Keeper gets debt-value + keeper-share
                    (keeper-payout (+ debt-value-in-collateral keeper-share))
                    
                    ;; Protocol gets protocol-share
                    (protocol-payout protocol-share)
                    
                    (final-keeper-payout (if (> total-reward collateral) collateral keeper-payout))
                    (final-protocol-payout (if (> total-reward collateral) u0 protocol-payout))
                )
                (try! (contract-call? .stable-token burn-for-vault debt tx-sender))

                (try! (as-contract (contract-call? collateral-token transfer final-keeper-payout tx-sender liquidator none)))
                
                ;; Update Reserve
                (map-set stability-reserve token-principal (+ (default-to u0 (map-get? stability-reserve token-principal)) final-protocol-payout))

                (map-delete vaults { owner: target, collateral-token: token-principal })
                
                (map-set supported-collateral token-principal (merge type-info { total-debt: (- (get total-debt type-info) debt) }))

                (ok final-keeper-payout)
            )
        )
    )
)

(define-public (partial-liquidate (target principal) (collateral-token <sip010-token>) (oracle <oracle-trait>))
    (let (
            (token-principal (contract-of collateral-token))
        )
        (try! (accrue-interest token-principal))
        (let (
                (liquidator tx-sender)
                (type-info (unwrap! (map-get? supported-collateral token-principal) err-collateral-not-supported))
                (vault (get-vault target token-principal))
                (price (unwrap! (get-price-from-oracle token-principal oracle) err-oracle-error))
                (collateral (get collateral vault))
                (current-debt (get-total-debt (get normalized-debt vault) (get current-index type-info)))
                (collateral-val (calculate-collateral-value collateral price))
                (current-ratio (if (is-eq current-debt u0) u99999999 (/ (* collateral-val u100) current-debt)))
            )
            (asserts! (< current-ratio (get liquidation-ratio type-info)) err-liquidation-not-allowed)
            
            (if (< current-ratio critical-ratio)
                (liquidate-full target collateral-token oracle)
                (let (
                        (target-debt (/ (* collateral-val u100) target-ratio-after-liquidation))
                        (debt-to-repay (- current-debt target-debt))
                        (debt-value-in-collateral (/ (* debt-to-repay u100000000) price))
                        
                        (total-penalty (/ (* debt-value-in-collateral liquidation-penalty) u100))
                        
                        ;; Split
                        (keeper-share (/ (* total-penalty (var-get keeper-reward-ratio)) u100))
                        (protocol-share (- total-penalty keeper-share))
                        
                        (reward-collateral (+ debt-value-in-collateral keeper-share))
                        (total-seized (+ reward-collateral protocol-share))
                        
                        (new-actual-debt target-debt)
                        (new-collateral (- collateral total-seized))
                        (current-index (get current-index type-info))
                        (repaid-normalized (/ (* debt-to-repay scale-factor) current-index))
                        (new-normalized-debt (- (get normalized-debt vault) repaid-normalized))
                    )
                    (try! (contract-call? .stable-token burn-for-vault debt-to-repay tx-sender))
                    
                    (try! (as-contract (contract-call? collateral-token transfer reward-collateral tx-sender liquidator none)))
                    
                    ;; Update Reserve
                    (map-set stability-reserve token-principal (+ (default-to u0 (map-get? stability-reserve token-principal)) protocol-share))

                    (map-set vaults { owner: target, collateral-token: token-principal } {
                        collateral: new-collateral,
                        normalized-debt: new-normalized-debt
                    })
                    
                    (map-set supported-collateral token-principal (merge type-info { total-debt: (- (get total-debt type-info) debt-to-repay) }))
                    
                    (ok reward-collateral)
                )
            )
        )
    )
)

(define-public (liquidate (target principal) (collateral-token <sip010-token>) (oracle <oracle-trait>) )
    (liquidate-full target collateral-token oracle)
)

(define-public (flash-mint
        (amount uint)
        (flash-loan-contract <flash-loan-trait>)
    )
    (let (
            (fee (/ (* amount flash-mint-fee) u10000))
            (total-repay (+ amount fee))
        )
        (try! (contract-call? .stable-token mint-for-vault amount tx-sender))
        (try! (contract-call? flash-loan-contract execute amount))
        (try! (contract-call? .stable-token burn-for-vault total-repay tx-sender))
        (ok total-repay)
    )
)
