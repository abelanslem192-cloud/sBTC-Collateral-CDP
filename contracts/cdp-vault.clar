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

;; Math Constants
(define-constant liquidation-ratio u150) ;; 150% collateralization ratio
(define-constant liquidation-penalty u10) ;; 10% penalty
(define-constant oracle-decimals u8)
(define-constant flash-mint-fee u10) ;; 0.1% fee (basis points)

;; Data Vars
(define-data-var sbtc-price uint u50000000000) ;; $50,000 * 10^6 (mock price with 6 decimals for simplicity matching stablecoin)
(define-data-var shutdown-activated bool false) ;; Circuit Breaker

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
    ;; collateral (8 decimals) * price (6 decimals) / 10^8 = value in 6 decimals (stablecoin match)
    (/ (* collateral-amount (var-get sbtc-price)) u100000000)
)

(define-read-only (calculate-current-ratio (user principal))
    (let (
            (vault (get-vault user))
            (collateral-val (calculate-collateral-value (get collateral vault)))
            (debt (get debt vault))
        )
        (if (is-eq debt u0)
            u99999999 ;; Infinite ratio if no debt
            (/ (* collateral-val u100) debt) ;; Ratio in percentage
        )
    )
)

;; Public Functions

;; Admin: Set Price
(define-public (set-price (new-price uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set sbtc-price new-price)
        (ok true)
    )
)

;; Admin: Toggle Circuit Breaker
(define-public (toggle-shutdown (shutdown bool))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set shutdown-activated shutdown)
        (ok shutdown)
    )
)

(define-read-only (is-shutdown)
    (var-get shutdown-activated)
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
        )
        ;; Check if new debt keeps ratio above 150%
        (asserts! (>= (/ (* collateral-val u100) new-debt) liquidation-ratio)
            err-under-collateralized
        )

        ;; Mint stablecoin to user
        ;; Note: cdp-vault must be authorized minter in stable-token
        (try! (contract-call? .stable-token mint-for-vault amount tx-sender))

        (map-set vaults tx-sender {
            collateral: (get collateral vault),
            debt: new-debt,
        })
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

        ;; Burn stablecoin from user
        (try! (contract-call? .stable-token burn-for-vault amount tx-sender))

        (map-set vaults tx-sender {
            collateral: (get collateral vault),
            debt: (- current-debt amount),
        })
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
        )
        (asserts! (>= current-collateral amount) err-insufficient-collateral)

        ;; Check if withdrawal keeps ratio above 150% (if debt exists)
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

;; 5. Liquidate (Simplified: Instant full liquidation)
(define-public (liquidate (target principal))
    (let (
            (vault (get-vault target))
            (collateral (get collateral vault))
            (debt (get debt vault))
            (ratio (calculate-current-ratio target))
        )
        ;; Check if ratio is below 150%
        (asserts! (< ratio liquidation-ratio) err-liquidation-not-allowed)

        ;; Liquidator pays off debt by burning stablecoin
        ;; Liquidator receives collateral minus penalty? 
        ;; Simplified: Liquidator pays debt, gets equivalent collateral + 10% bonus
        ;; But wait, if they pay all debt, they might get more collateral than debt value.
        ;; Let's assume liquidator pays `debt` amount of stablecoin.
        ;; Liquidator MUST receive collateral worth `debt * 1.10`.
        ;; If vault doesn't have enough collateral, they get everything (bad debt protocol loss, but we ignore for MVP).

        (let (
                (debt-value-in-collateral (/ (* debt u100000000) (var-get sbtc-price))) ;; Convert debt back to sBTC satoshis
                (reward-collateral (/ (* debt-value-in-collateral (+ u100 liquidation-penalty)) u100))
                (actual-reward (if (> reward-collateral collateral)
                    collateral
                    reward-collateral
                ))
            )
            ;; Burn debt from liquidator
            (try! (contract-call? .stable-token burn-for-vault debt tx-sender))

            ;; Send collateral to liquidator
            (try! (as-contract (contract-call? .sbtc-token transfer actual-reward tx-sender
                tx-sender none
            )))

            ;; Clear vault
            (map-delete vaults target)

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
            ;; Calculate fee (0.1%)
            (fee (/ (* amount flash-mint-fee) u10000))
            (total-repay (+ amount fee))
        )
        ;; Mint stablecoin to caller (optimistic minting)
        (try! (contract-call? .stable-token mint-for-vault amount tx-sender))

        ;; Execute callback on borrower contract
        ;; Borrower must use funds and approve contract to burn total-repay amount
        (try! (contract-call? flash-loan-contract execute amount))

        ;; Burn principal + fee from borrower
        ;; If borrower doesn't have enough, this fails and reverts entire tx
        (try! (contract-call? .stable-token burn-for-vault total-repay tx-sender))

        (ok total-repay)
    ))
)
