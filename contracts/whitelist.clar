(use .addr-utils)
(use .time-utils)

;; ========================
;; CONSTANTS & CONFIGURATION
;; ========================
(define-constant MAX_WHITELIST_SIZE u1000)
(define-constant DEFAULT_EXPIRATION u5760)

;; Error codes
(define-constant ERR_ALREADY_WHITELISTED u100)
(define-constant ERR_NOT_WHITELISTED u101)
(define-constant ERR_NOT_AUTHORIZED u102)
(define-constant ERR_MAX_CAPACITY u103)
(define-constant ERR_CONTRACT_NOT_ALLOWED u104)
(define-constant ERR_EXPIRED u105)
(define-constant ERR_NOT_ACTIVE u106)

;; ========================
;; DATA STORAGE
;; ========================
(define-data-var contract-owner principal tx-sender)
(define-data-var whitelist-active bool true)
(define-data-var allow-contracts bool false)
(define-data-var whitelist-count uint u0)

(define-map whitelist principal {
  added-by: principal,
  added-at: uint,
  expires-at: uint
})

(define-map managers principal bool)

;; ========================
;; EVENTS
;; ========================
(define-event WhitelistAdded (user principal) (manager principal) (expires uint))
(define-event WhitelistRemoved (user principal) (manager principal))
(define-event ContractToggled (status bool))
(define-event WhitelistToggled (status bool))

;; ========================
;; CORE FUNCTIONS
;; ========================
(define-public (add-to-whitelist (user principal) (expiration (optional uint)))
  (let ((expires (default-to (+ block-height DEFAULT_EXPIRATION) expiration)))
    (begin
      (asserts! (or (is-eq tx-sender (var-get contract-owner))
                    (is-eq (unwrap-panic (map-get? managers tx-sender)) true))
                (err ERR_NOT_AUTHORIZED))

      (asserts! (var-get whitelist-active) (err ERR_NOT_ACTIVE))

      (if (and (contract-principal? user) (not (var-get allow-contracts)))
          (err ERR_CONTRACT_NOT_ALLOWED)
          (begin
            (asserts! (is-none (map-get? whitelist user)) (err ERR_ALREADY_WHITELISTED))
            (asserts! (< (var-get whitelist-count) MAX_WHITELIST_SIZE) (err ERR_MAX_CAPACITY))

            (map-set whitelist user {
              added-by: tx-sender,
              added-at: block-height,
              expires-at: expires
            })

            (var-set whitelist-count (+ (var-get whitelist-count) u1))
            (emit-event WhitelistAdded user tx-sender expires)
            (ok true)
          )
      )
    )
  )
)

(define-public (batch-add (users (list 50 principal)) (expiration (optional uint)))
  (let ((expires (default-to (+ block-height DEFAULT_EXPIRATION) expiration)))
    (begin
      (asserts! (or (is-eq tx-sender (var-get contract-owner))
                    (is-eq (unwrap-panic (map-get? managers tx-sender)) true))
                (err ERR_NOT_AUTHORIZED))
      (asserts! (var-get whitelist-active) (err ERR_NOT_ACTIVE))

      (begin
        (map
          (lambda (user)
            (begin
              (if (and (contract-principal? user) (not (var-get allow-contracts)))
                  none
                  (match (map-get? whitelist user)
                    entry none
                    (begin
                      (if (< (var-get whitelist-count) MAX_WHITELIST_SIZE)
                          (begin
                            (map-set whitelist user {
                              added-by: tx-sender,
                              added-at: block-height,
                              expires-at: expires
                            })
                            (var-set whitelist-count (+ (var-get whitelist-count) u1))
                            (emit-event WhitelistAdded user tx-sender expires)
                            none
                          )
                          none
                      )
                    )
                  )
              )
            )
          )
          users
        )
        (ok true)
      )
    )
  )
)

(define-public (remove-from-whitelist (user principal))
  (begin
    (asserts! (or (is-eq tx-sender (var-get contract-owner))
                  (is-eq (unwrap-panic (map-get? managers tx-sender)) true))
              (err ERR_NOT_AUTHORIZED))

    (asserts! (is-some (map-get? whitelist user)) (err ERR_NOT_WHITELISTED))

    (map-delete whitelist user)
    (var-set whitelist-count (- (var-get whitelist-count) u1))
    (emit-event WhitelistRemoved user tx-sender)
    (ok true)
  )
)

(define-public (batch-remove (users (list 50 principal)))
  (begin
    (asserts! (or (is-eq tx-sender (var-get contract-owner))
                  (is-eq (unwrap-panic (map-get? managers tx-sender)) true))
              (err ERR_NOT_AUTHORIZED))

    (begin
      (map
        (lambda (user)
          (match (map-get? whitelist user)
            entry
            (begin
              (map-delete whitelist user)
              (var-set whitelist-count (- (var-get whitelist-count) u1))
              (emit-event WhitelistRemoved user tx-sender)
              none
            )
            none none
          )
        )
        users
      )
      (ok true)
    )
  )
)

;; ========================
;; ADMIN FUNCTIONS
;; ========================
(define-public (set-manager (manager principal) (enabled bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_NOT_AUTHORIZED))
    (map-set managers manager enabled)
    (ok true)
  )
)

(define-public (toggle-allow-contracts (status bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_NOT_AUTHORIZED))
    (var-set allow-contracts status)
    (emit-event ContractToggled status)
    (ok true)
  )
)

(define-public (toggle-whitelist (status bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_NOT_AUTHORIZED))
    (var-set whitelist-active status)
    (emit-event WhitelistToggled status)
    (ok true)
  )
)

(define-public (set-expiration (user principal) (expiration uint))
  (begin
    (asserts! (or (is-eq tx-sender (var-get contract-owner))
                  (is-eq (unwrap-panic (map-get? managers tx-sender)) true))
              (err ERR_NOT_AUTHORIZED))

    (let ((entry (unwrap! (map-get? whitelist user) (err ERR_NOT_WHITELISTED))))
      (map-set whitelist user (merge entry {expires-at: expiration}))
      (ok true)
    )
  )
)

;; ========================
;; READ-ONLY FUNCTIONS
;; ========================
(define-read-only (is-whitelisted (user principal))
  (match (map-get? whitelist user)
    entry
    (ok (and (>= block-height (get added-at entry)) (<= block-height (get expires-at entry))))
    none (ok false)
  )
)

(define-read-only (get-whitelist-entry (user principal))
  (map-get? whitelist user)
)

(define-read-only (get-whitelist-size)
  (ok (var-get whitelist-count))
)

(define-read-only (is-manager (addr principal))
  (ok (map-get? managers addr))
)

(define-read-only (whitelist-status)
  (ok {
    active: (var-get whitelist-active),
    allow-contracts: (var-get allow-contracts),
    count: (var-get whitelist-count),
    max-size: MAX_WHITELIST_SIZE
  })
)

;; ========================
;; (LIMITED) UTILITY FUNCTION
;; ========================
(define-public (prune-expired)
  ;; Clarity can't iterate full maps, this must be handled off-chain or with fixed keys
  (err u999) ;; Not supported in Clarity natively
)

