;; sbtc-token.clar
;; Mock sBTC token (SIP-010)

(impl-trait .sip-010-trait.sip-010-trait)

(define-fungible-token sbtc)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))

(define-public (transfer
        (amount uint)
        (sender principal)
        (recipient principal)
        (memo (optional (buff 34)))
    )
    (begin
        (asserts! (is-eq tx-sender sender) err-not-token-owner)
        (try! (ft-transfer? sbtc amount sender recipient))
        (match memo
            to-print (print to-print)
            0x
        )
        (ok true)
    )
)

(define-read-only (get-name)
    (ok "sBTC")
)

(define-read-only (get-symbol)
    (ok "sBTC")
)

(define-read-only (get-decimals)
    (ok u8)
)

(define-read-only (get-balance (who principal))
    (ok (ft-get-balance sbtc who))
)

(define-read-only (get-total-supply)
    (ok (ft-get-supply sbtc))
)

(define-read-only (get-token-uri)
    (ok none)
)

;; Faucet for testing
(define-public (faucet-mint (amount uint))
    (ft-mint? sbtc amount tx-sender)
)
