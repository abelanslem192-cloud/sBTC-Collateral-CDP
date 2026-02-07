;; stable-token.clar
;; Stablecoin token (SIP-010) mintable by vault

(impl-trait .sip-010-trait.sip-010-trait)

(define-fungible-token stable-coin)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-unauthorized-minter (err u102))

(define-data-var vault-principal principal tx-sender)

(define-public (set-vault (new-vault principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set vault-principal new-vault)
        (ok true)
    )
)

(define-public (transfer
        (amount uint)
        (sender principal)
        (recipient principal)
        (memo (optional (buff 34)))
    )
    (begin
        (asserts! (is-eq tx-sender sender) err-not-token-owner)
        (try! (ft-transfer? stable-coin amount sender recipient))
        (match memo
            to-print (print to-print)
            0x
        )
        (ok true)
    )
)

(define-read-only (get-name)
    (ok "Stable Coin")
)

(define-read-only (get-symbol)
    (ok "STBL")
)

(define-read-only (get-decimals)
    (ok u6)
)

(define-read-only (get-balance (who principal))
    (ok (ft-get-balance stable-coin who))
)

(define-read-only (get-total-supply)
    (ok (ft-get-supply stable-coin))
)

(define-read-only (get-token-uri)
    (ok none)
)

;; Mint/Burn only by vault
(define-public (mint-for-vault
        (amount uint)
        (recipient principal)
    )
    (begin
        (asserts! (is-eq tx-sender (var-get vault-principal))
            err-unauthorized-minter
        )
        (ft-mint? stable-coin amount recipient)
    )
)

(define-public (burn-for-vault
        (amount uint)
        (sender principal)
    )
    (begin
        (asserts! (is-eq tx-sender (var-get vault-principal))
            err-unauthorized-minter
        )
        (ft-burn? stable-coin amount sender)
    )
)
