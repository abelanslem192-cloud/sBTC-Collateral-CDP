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
(define-constant err-shutdown (err u108))
(define-constant err-debt-ceiling-reached (err u109))

;; Math Constants
(define-constant liquidation-penalty u10) ;; 10% penalty
(define-constant oracle-decimals u8)
(define-constant flash-mint-fee u10) ;; 0.1% fee (basis points)

;; Feature 9: Collateral Ratio Tiers (debt thresholds in 6 decimals)
;; Tier 1: debt <= 10K STBL -> 150% ratio
;; Tier 2: debt <= 100K STBL -> 175% ratio
;; Tier 3: debt > 100K STBL -> 200% ratio
(define-constant tier-1-max u10000000000)   ;; 10,000 STBL
(define-constant tier-2-max u100000000000)  ;; 100,000 STBL
(define-constant tier-1-ratio u150)
(define-constant tier-2-ratio u175)
(define-constant tier-3-ratio u200)

;; Data Vars
(define-data-var sbtc-price uint u50000000000) ;; $50,000 * 10^6
(define-data-var shutdown-activated bool false) ;; Feature 7: Circuit Breaker
(define-data-var debt-ceiling uint u1000000000000) ;; Feature 8: Default 1M STBL
(define-data-var total-system-debt uint u0) ;; Feature 8: Global debt tracker

;; Maps
(define-map vaults
    principal
    {
        collateral: uint,
        debt: uint,
    }
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

(define-read-only (calculate-collateral-value (collateral-amount uint))
    (/ (* collateral-amount (var-get sbtc-price)) u100000000)
)

;; Feature 9: Get required ratio based on debt size
(define-read-only (get-required-ratio (debt uint))
    (if (<= debt tier-1-max)
        tier-1-ratio
        (if (<= debt tier-2-max)
            tier-2-ratio
            tier-3-ratio
        )
    )
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

(define-read-only (is-shutdown)
    (var-get shutdown-activated)
)

(define-read-only (get-debt-ceiling)
    (var-get debt-ceiling)
)

(define-read-only (get-total-system-debt)
    (var-get total-system-debt)
)

;; Admin Functions

(define-public (set-price (new-price uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set sbtc-price new-price)
        (ok true)
    )
)

(define-public (toggle-shutdown (shutdown bool))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set shutdown-activated shutdown)
        (ok shutdown)
    )
)

(define-public (set-debt-ceiling (new-ceiling uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set debt-ceiling new-ceiling)
        (ok new-ceiling)
    )
)

;; 1. Deposit Collateral
(define-public (deposit-collateral (amount uint))
    (begin
        (asserts! (not (var-get shutdown-activated)) err-shutdown)
    (let (
            (vault (get-vault tx-sender))
            (current-collateral (get collateral vault))
            (new-collateral (+ current-collateral amount))
        )
        (try! (contract-call? .sbtc-token transfer amount tx-sender
            (as-contract tx-sender) none
        ))
        (map-set vaults tx-sender {
            collateral: new-collateral,
            debt: (get debt vault),
        })
        (ok new-collateral)
    ))
)

;; 2. Borrow (Mint Stablecoin)
(define-public (borrow (amount uint))
    (begin
        (asserts! (not (var-get shutdown-activated)) err-shutdown)
    (let (
            (vault (get-vault tx-sender))
            (current-debt (get debt vault))
            (new-debt (+ current-debt amount))
            (collateral-val (calculate-collateral-value (get collateral vault)))
            (required-ratio (get-required-ratio new-debt))
        )
        ;; Check collateralization ratio (tiered)
        (asserts! (>= (/ (* collateral-val u100) new-debt) required-ratio)
            err-under-collateralized
        )

        ;; Check debt ceiling
        (asserts! (<= (+ (var-get total-system-debt) amount) (var-get debt-ceiling))
            err-debt-ceiling-reached
        )

        ;; Mint stablecoin to user
        (try! (contract-call? .stable-token mint-for-vault amount tx-sender))

        (map-set vaults tx-sender {
            collateral: (get collateral vault),
            debt: new-debt,
        })
        (var-set total-system-debt (+ (var-get total-system-debt) amount))
        (ok new-debt)
    ))
)

;; 3. Repay (Burn Stablecoin)
(define-public (repay (amount uint))
    (let (
            (vault (get-vault tx-sender))
            (current-debt (get debt vault))
        )
        (asserts! (<= amount current-debt) err-repayment-too-high)

        (try! (contract-call? .stable-token burn-for-vault amount tx-sender))

        (map-set vaults tx-sender {
            collateral: (get collateral vault),
            debt: (- current-debt amount),
        })
        (var-set total-system-debt (- (var-get total-system-debt) amount))
        (ok (- current-debt amount))
    )
)

;; 4. Withdraw Collateral
(define-public (withdraw-collateral (amount uint))
    (let (
            (vault (get-vault tx-sender))
            (current-collateral (get collateral vault))
            (new-collateral (- current-collateral amount))
            (debt (get debt vault))
            (collateral-val (calculate-collateral-value new-collateral))
            (required-ratio (get-required-ratio debt))
        )
        (asserts! (>= current-collateral amount) err-insufficient-collateral)

        ;; Check tiered ratio (if debt exists)
        (asserts!
            (or (is-eq debt u0) (>= (/ (* collateral-val u100) debt) required-ratio))
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

;; 5. Liquidate
(define-public (liquidate (target principal))
    (let (
            (vault (get-vault target))
            (collateral (get collateral vault))
            (debt (get debt vault))
            (ratio (calculate-current-ratio target))
            (required-ratio (get-required-ratio debt))
        )
        ;; Check if ratio is below the tiered requirement
        (asserts! (< ratio required-ratio) err-liquidation-not-allowed)

        (let (
                (debt-value-in-collateral (/ (* debt u100000000) (var-get sbtc-price)))
                (reward-collateral (/ (* debt-value-in-collateral (+ u100 liquidation-penalty)) u100))
                (actual-reward (if (> reward-collateral collateral)
                    collateral
                    reward-collateral
                ))
            )
            (try! (contract-call? .stable-token burn-for-vault debt tx-sender))

            (try! (as-contract (contract-call? .sbtc-token transfer actual-reward tx-sender
                tx-sender none
            )))

            (map-delete vaults target)
            (var-set total-system-debt (- (var-get total-system-debt) debt))

            (ok actual-reward)
        )
    )
)

;; 6. Flash Mint
(define-public (flash-mint
        (amount uint)
        (flash-loan-contract <flash-loan-trait>)
    )
    (begin
        (asserts! (not (var-get shutdown-activated)) err-shutdown)
    (let (
            (fee (/ (* amount flash-mint-fee) u10000))
            (total-repay (+ amount fee))
        )
        (try! (contract-call? .stable-token mint-for-vault amount tx-sender))
        (try! (contract-call? flash-loan-contract execute amount))
        (try! (contract-call? .stable-token burn-for-vault total-repay tx-sender))

        (ok total-repay)
    ))
)
