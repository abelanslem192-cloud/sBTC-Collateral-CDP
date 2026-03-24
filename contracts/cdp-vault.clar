;; cdp-vault.clar
;; Core CDP logic: Lock sBTC, mint Stablecoin

(use-trait sip010-token .sip-010-trait.sip-010-trait)
(use-trait stable-token-trait .sip-010-trait.sip-010-trait)
(use-trait flash-loan-trait .flash-loan-trait.flash-loan-trait)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-collateral (err u101))
(define-constant err-vault-exists (err u102))
(define-constant err-vault-not-found (err u103))
(define-constant err-under-collateralized (err u104))
(define-constant err-repayment-too-high (err u105))
(define-constant err-liquidation-not-allowed (err u106))
(define-constant err-transfer-failed (err u107))
(define-constant err-merge-self (err u108))
(define-constant err-empty-source (err u109))
(define-constant err-paused (err u110))
(define-constant err-debt-ceiling-reached (err u111))

;; Math Constants
(define-constant liquidation-ratio u150)
(define-constant liquidation-penalty-liquidator u8)
(define-constant liquidation-penalty-protocol u2)
(define-constant oracle-decimals u8)
(define-constant flash-mint-fee u10)

;; Data Vars
(define-data-var sbtc-price uint u50000000000)
(define-data-var is-paused bool false)
(define-data-var total-debt uint u0)
(define-data-var debt-ceiling uint u1000000000000)
(define-data-var protocol-treasury principal contract-owner)

;; Maps
(define-map vaults
    principal
    {
        collateral: uint,
        debt: uint,
    }
)

(define-map merge-approvals
    { source: principal, destination: principal }
    bool
)

;; Read-Only Functions
(define-read-only (get-vault (user principal))
    (default-to {
        collateral: u0,
        debt: u0,
    }
        (map-get? vaults user)
    )
)

(define-read-only (get-btc-price)
    (var-get sbtc-price)
)

(define-read-only (get-total-debt)
    (var-get total-debt)
)

(define-read-only (calculate-collateral-value (collateral-amount uint))
    (/ (* collateral-amount (var-get sbtc-price)) u100000000)
)

(define-read-only (calculate-current-ratio (user principal))
    (let (
            (vault (get-vault user))
            (collateral-val (calculate-collateral-value (get collateral vault)))
            (debt (get debt vault))
        )
        (if (is-eq debt u0)
            u99999999
            (/ (* collateral-val u100) debt)
        )
    )
)

(define-read-only (is-merge-approved (source principal) (destination principal))
    (default-to false (map-get? merge-approvals { source: source, destination: destination }))
)

;; Public Functions

(define-public (set-paused (paused bool))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set is-paused paused))
    )
)

(define-public (set-debt-ceiling (ceiling uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set debt-ceiling ceiling))
    )
)

(define-public (set-protocol-treasury (treasury principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set protocol-treasury treasury))
    )
)

(define-public (set-price (new-price uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set sbtc-price new-price)
        (ok true)
    )
)

(define-public (deposit-collateral (amount uint))
    (let (
            (vault (get-vault tx-sender))
            (current-collateral (get collateral vault))
            (new-collateral (+ current-collateral amount))
        )
        (asserts! (not (var-get is-paused)) err-paused)
        (try! (contract-call? .sbtc-token transfer amount tx-sender
            (as-contract tx-sender) none
        ))
        (map-set vaults tx-sender {
            collateral: new-collateral,
            debt: (get debt vault),
        })
        (ok new-collateral)
    )
)

(define-public (borrow (amount uint))
    (let (
            (vault (get-vault tx-sender))
            (current-debt (get debt vault))
            (new-debt (+ current-debt amount))
            (collateral-val (calculate-collateral-value (get collateral vault)))
        )
        (asserts! (not (var-get is-paused)) err-paused)
        (asserts! (<= (+ (var-get total-debt) amount) (var-get debt-ceiling)) err-debt-ceiling-reached)
        (asserts! (>= (/ (* collateral-val u100) new-debt) liquidation-ratio) err-under-collateralized)

        (try! (contract-call? .stable-token mint-for-vault amount tx-sender))

        (map-set vaults tx-sender {
            collateral: (get collateral vault),
            debt: new-debt,
        })
        (var-set total-debt (+ (var-get total-debt) amount))
        (ok new-debt)
    )
)

(define-public (repay (amount uint))
    (let (
            (vault (get-vault tx-sender))
            (current-debt (get debt vault))
        )
        (asserts! (not (var-get is-paused)) err-paused)
        (asserts! (<= amount current-debt) err-repayment-too-high)

        (try! (contract-call? .stable-token burn-for-vault amount tx-sender))

        (map-set vaults tx-sender {
            collateral: (get collateral vault),
            debt: (- current-debt amount),
        })
        (var-set total-debt (- (var-get total-debt) amount))
        (ok (- current-debt amount))
    )
)

(define-public (withdraw-collateral (amount uint))
    (let (
            (vault (get-vault tx-sender))
            (current-collateral (get collateral vault))
            (new-collateral (- current-collateral amount))
            (debt (get debt vault))
            (collateral-val (calculate-collateral-value new-collateral))
        )
        (asserts! (not (var-get is-paused)) err-paused)
        (asserts! (>= current-collateral amount) err-insufficient-collateral)

        (asserts!
            (or (is-eq debt u0) (>= (/ (* collateral-val u100) debt) liquidation-ratio))
            err-under-collateralized
        )

        (try! (as-contract (contract-call? .sbtc-token transfer amount tx-sender tx-sender none)))

        (map-set vaults tx-sender {
            collateral: new-collateral,
            debt: debt,
        })
        (ok new-collateral)
    )
)

(define-public (liquidate (target principal))
    (let (
            (vault (get-vault target))
            (collateral (get collateral vault))
            (debt (get debt vault))
            (ratio (calculate-current-ratio target))
        )
        (asserts! (not (var-get is-paused)) err-paused)
        (asserts! (< ratio liquidation-ratio) err-liquidation-not-allowed)

        (let (
                (debt-value-in-collateral (/ (* debt u100000000) (var-get sbtc-price)))
                (penalty-total (+ liquidation-penalty-liquidator liquidation-penalty-protocol))
                (reward-collateral (/ (* debt-value-in-collateral (+ u100 penalty-total)) u100))
                (actual-reward (if (> reward-collateral collateral) collateral reward-collateral))
                
                (liquidator-base (/ (* debt-value-in-collateral (+ u100 liquidation-penalty-liquidator)) u100))
                (liquidator-reward (if (> liquidator-base actual-reward) actual-reward liquidator-base))
                (protocol-reward (- actual-reward liquidator-reward))
            )
            (try! (contract-call? .stable-token burn-for-vault debt tx-sender))

            (try! (as-contract (contract-call? .sbtc-token transfer liquidator-reward tx-sender tx-sender none)))
            
            (if (> protocol-reward u0)
                (try! (as-contract (contract-call? .sbtc-token transfer protocol-reward tx-sender (var-get protocol-treasury) none)))
                false
            )

            (map-delete vaults target)
            (var-set total-debt (- (var-get total-debt) debt))

            (ok actual-reward)
        )
    )
)

(define-public (flash-mint
        (amount uint)
        (flash-loan-contract <flash-loan-trait>)
    )
    (let (
            (fee (/ (* amount flash-mint-fee) u10000))
            (total-repay (+ amount fee))
        )
        (asserts! (not (var-get is-paused)) err-paused)
        (asserts! (<= (+ (var-get total-debt) amount) (var-get debt-ceiling)) err-debt-ceiling-reached)
        
        (var-set total-debt (+ (var-get total-debt) amount))
        
        (try! (contract-call? .stable-token mint-for-vault amount tx-sender))
        (try! (contract-call? flash-loan-contract execute amount))
        (try! (contract-call? .stable-token burn-for-vault total-repay tx-sender))
        
        (var-set total-debt (- (var-get total-debt) amount))
        (ok total-repay)
    )
)

(define-public (approve-merge (destination principal))
    (begin
        (asserts! (not (var-get is-paused)) err-paused)
        (asserts! (not (is-eq tx-sender destination)) err-merge-self)
        (ok (map-set merge-approvals { source: tx-sender, destination: destination } true))
    )
)

(define-public (revoke-merge (destination principal))
    (begin
        (asserts! (not (var-get is-paused)) err-paused)
        (ok (map-delete merge-approvals { source: tx-sender, destination: destination }))
    )
)

(define-public (merge-vault (source principal))
    (let (
            (source-vault (get-vault source))
            (dest-vault (get-vault tx-sender))
            (source-collateral (get collateral source-vault))
            (source-debt (get debt source-vault))
            (dest-collateral (get collateral dest-vault))
            (dest-debt (get debt dest-vault))
            (merged-collateral (+ dest-collateral source-collateral))
            (merged-debt (+ dest-debt source-debt))
            (merged-collateral-val (calculate-collateral-value merged-collateral))
        )
        (asserts! (not (var-get is-paused)) err-paused)
        (asserts! (not (is-eq tx-sender source)) err-merge-self)
        (asserts! (is-merge-approved source tx-sender) err-owner-only)
        (asserts! (or (> source-collateral u0) (> source-debt u0)) err-empty-source)

        (asserts!
            (or (is-eq merged-debt u0) (>= (/ (* merged-collateral-val u100) merged-debt) liquidation-ratio))
            err-under-collateralized
        )

        (map-set vaults tx-sender {
            collateral: merged-collateral,
            debt: merged-debt,
        })
        (map-delete vaults source)
        (map-delete merge-approvals { source: source, destination: tx-sender })

        (ok { collateral: merged-collateral, debt: merged-debt })
    )
)
