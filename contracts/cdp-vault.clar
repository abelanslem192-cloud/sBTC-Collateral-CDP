(use-trait sip010-token .sip-010-trait.sip-010-trait)
(use-trait stable-token-trait .sip-010-trait.sip-010-trait)
(use-trait flash-loan-trait .flash-loan-trait.flash-loan-trait)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-collateral (err u101))
(define-constant err-vault-exists (err u102))
(define-constant err-vault-not-found (err u103))
(define-constant err-under-collateralized (err u104))
(define-constant err-repayment-too-high (err u105))
(define-constant err-liquidation-not-allowed (err u106))
(define-constant err-transfer-failed (err u107))

(define-constant liquidation-ratio u150)
(define-constant liquidation-penalty u10)
(define-constant oracle-decimals u8)
(define-constant flash-mint-fee u10)
(define-constant interest-rate-per-block u150)
(define-constant scale-factor u100000000)
(define-constant target-ratio-after-liquidation u175)
(define-constant critical-ratio u100)

(define-data-var sbtc-price uint u50000000000)

(define-map vaults
    principal
    {
        collateral: uint,
        debt: uint,
        last-accrued-block: uint
    }
)

(define-private (calculate-accrued-interest (debt uint) (last-accrued-block uint))
    (if (or (is-eq debt u0) (is-eq last-accrued-block u0))
        u0
        (let (
            (delta-blocks (- burn-block-height last-accrued-block))
            (interest (/ (* debt interest-rate-per-block delta-blocks) scale-factor))
        )
        interest)
    )
)

(define-read-only (get-vault (user principal))
    (default-to {
        collateral: u0,
        debt: u0,
        last-accrued-block: burn-block-height
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

(define-read-only (calculate-current-ratio (user principal))
    (let (
            (vault (get-vault user))
            (collateral-val (calculate-collateral-value (get collateral vault)))
            (pending-interest (calculate-accrued-interest (get debt vault) (get last-accrued-block vault)))
            (total-debt (+ (get debt vault) pending-interest))
        )
        (if (is-eq total-debt u0)
            u99999999
            (/ (* collateral-val u100) total-debt)
        )
    )
)

(define-read-only (calculate-partial-liquidation-amount (target principal))
    (let (
            (vault (get-vault target))
            (collateral (get collateral vault))
            (pending-interest (calculate-accrued-interest (get debt vault) (get last-accrued-block vault)))
            (current-debt (+ (get debt vault) pending-interest))
            (collateral-val (calculate-collateral-value collateral))
        )
        (if (is-eq current-debt u0)
            (ok u0)
            (let (
                    (target-debt (/ (* collateral-val u100) target-ratio-after-liquidation))
                    (debt-to-repay (if (> current-debt target-debt) (- current-debt target-debt) u0))
                )
                (ok debt-to-repay)
            )
        )
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
            (pending-interest (calculate-accrued-interest (get debt vault) (get last-accrued-block vault)))
            (new-debt (+ (get debt vault) pending-interest))
        )
        (try! (contract-call? .sbtc-token transfer amount tx-sender
            (as-contract tx-sender) none
        ))
        (map-set vaults tx-sender {
            collateral: new-collateral,
            debt: new-debt,
            last-accrued-block: burn-block-height
        })
        (ok new-collateral)
    )
)

(define-public (borrow (amount uint))
    (let (
            (vault (get-vault tx-sender))
            (pending-interest (calculate-accrued-interest (get debt vault) (get last-accrued-block vault)))
            (current-debt (+ (get debt vault) pending-interest))
            (new-debt (+ current-debt amount))
            (collateral-val (calculate-collateral-value (get collateral vault)))
        )
        (asserts! (>= (/ (* collateral-val u100) new-debt) liquidation-ratio)
            err-under-collateralized
        )

        (try! (contract-call? .stable-token mint-for-vault amount tx-sender))

        (map-set vaults tx-sender {
            collateral: (get collateral vault),
            debt: new-debt,
            last-accrued-block: burn-block-height
        })
        (ok new-debt)
    )
)

(define-public (repay (amount uint))
    (let (
            (vault (get-vault tx-sender))
            (pending-interest (calculate-accrued-interest (get debt vault) (get last-accrued-block vault)))
            (current-debt (+ (get debt vault) pending-interest))
        )
        (asserts! (<= amount current-debt) err-repayment-too-high)

        (try! (contract-call? .stable-token burn-for-vault amount tx-sender))

        (map-set vaults tx-sender {
            collateral: (get collateral vault),
            debt: (- current-debt amount),
            last-accrued-block: burn-block-height
        })
        (ok (- current-debt amount))
    )
)

(define-public (withdraw-collateral (amount uint))
    (let (
            (vault (get-vault tx-sender))
            (current-collateral (get collateral vault))
            (new-collateral (- current-collateral amount))
            (pending-interest (calculate-accrued-interest (get debt vault) (get last-accrued-block vault)))
            (current-debt (+ (get debt vault) pending-interest))
            (collateral-val (calculate-collateral-value new-collateral))
        )
        (asserts! (>= current-collateral amount) err-insufficient-collateral)

        (asserts!
            (or (is-eq current-debt u0) (>= (/ (* collateral-val u100) current-debt) liquidation-ratio))
            err-under-collateralized
        )

        (try! (as-contract (contract-call? .sbtc-token transfer amount tx-sender tx-sender none)))

        (map-set vaults tx-sender {
            collateral: new-collateral,
            debt: current-debt,
            last-accrued-block: burn-block-height
        })
        (ok new-collateral)
    )
)

(define-public (partial-liquidate (target principal))
    (let (
            (vault (get-vault target))
            (collateral (get collateral vault))
            (pending-interest (calculate-accrued-interest (get debt vault) (get last-accrued-block vault)))
            (current-debt (+ (get debt vault) pending-interest))
            (collateral-val (calculate-collateral-value collateral))
            (current-ratio (if (is-eq current-debt u0) u99999999 (/ (* collateral-val u100) current-debt)))
        )
        (asserts! (< current-ratio liquidation-ratio) err-liquidation-not-allowed)
        
        (if (< current-ratio critical-ratio)
            (liquidate-full target)
            (let (
                    (target-debt (/ (* collateral-val u100) target-ratio-after-liquidation))
                    (debt-to-repay (- current-debt target-debt))
                    (debt-value-in-collateral (/ (* debt-to-repay u100000000) (var-get sbtc-price)))
                    (reward-collateral (/ (* debt-value-in-collateral (+ u100 liquidation-penalty)) u100))
                    (new-debt target-debt)
                    (new-collateral (- collateral reward-collateral))
                )
                (try! (contract-call? .stable-token burn-for-vault debt-to-repay tx-sender))
                
                (try! (as-contract (contract-call? .sbtc-token transfer reward-collateral tx-sender
                    tx-sender none
                )))

                (map-set vaults target {
                    collateral: new-collateral,
                    debt: new-debt,
                    last-accrued-block: burn-block-height
                })
                
                (ok reward-collateral)
            )
        )
    )
)

(define-public (liquidate (target principal))
    (liquidate-full target)
)

(define-private (liquidate-full (target principal))
    (let (
            (vault (get-vault target))
            (collateral (get collateral vault))
            (pending-interest (calculate-accrued-interest (get debt vault) (get last-accrued-block vault)))
            (debt (+ (get debt vault) pending-interest))
            (collateral-val (calculate-collateral-value collateral))
            (ratio (if (is-eq debt u0) u99999999 (/ (* collateral-val u100) debt)))
        )
        (asserts! (< ratio liquidation-ratio) err-liquidation-not-allowed)

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
        (try! (contract-call? .stable-token mint-for-vault amount tx-sender))

        (try! (contract-call? flash-loan-contract execute amount))

        (try! (contract-call? .stable-token burn-for-vault total-repay tx-sender))

        (ok total-repay)
    )
)