(impl-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip009-trait) ;; If tracking per NFT
(use .math-utils)

;; Error constants
(define-constant ERR_NOT_OWNER u100)
(define-constant ERR_INVALID_TYPE u101)
(define-constant ERR_INVALID_AMOUNT u102)
(define-constant ERR_MATH_ERROR u200)

;; Security constants
(define-constant MAX_IMPACT_AMOUNT u1000000000)

;; Contract ownership
(define-data-var contract-owner principal tx-sender)

;; Impact storage - separate maps for each metric
(define-map trees-planted (uint) uint)
(define-map co2-sequestered (uint) uint)  ;; Stored as tons
(define-map clean-energy (uint) uint)     ;; Stored as MW

;; Authorization check
(define-private (is-owner? (caller principal))
    (is-eq caller (var-get contract-owner))
)

;; Impact recording with type differentiation
(define-public (record-impact (token-id uint) (impact-type (string-ascii 20)) (amount uint))
    (begin
        ;; Authorization check
        (asserts! (is-owner? tx-sender) (err ERR_NOT_OWNER))
        
        ;; Input validation
        (asserts! (> amount u0) (err ERR_INVALID_AMOUNT))
        (asserts! (< amount MAX_IMPACT_AMOUNT) (err ERR_INVALID_AMOUNT))
        
        (let (
            (current (match impact-type
                "trees" (map-get? trees-planted token-id)
                "co2" (map-get? co2-sequestered token-id)
                "energy" (map-get? clean-energy token-id)
                (err ERR_INVALID_TYPE)
            ))
            (current-val (default-to u0 current))
            (new-total (unwrap! (contract-call? .math-utils safe-add current-val amount) (err ERR_MATH_ERROR)))
        )
        (match impact-type
            "trees" (map-set trees-planted token-id new-total)
            "co2" (map-set co2-sequestered token-id new-total)
            "energy" (map-set clean-energy token-id new-total)
        )
        (emit-event ImpactRecorded token-id impact-type amount)
        (ok true)
    )
))

;; Read functions
(define-read-only (get-trees (token-id uint))
    (map-get? trees-planted token-id)
)

(define-read-only (get-co2 (token-id uint))
    (map-get? co2-sequestered token-id)
)

(define-read-only (get-energy (token-id uint))
    (map-get? clean-energy token-id)
)

;; Combined getter
(define-read-only (get-all-impacts (token-id uint))
    (ok {
        trees: (map-get? trees-planted token-id),
        co2: (map-get? co2-sequestered token-id),
        energy: (map-get? clean-energy token-id)
    })
)
