(use .math-utils)
(use .time-utils)
(use-trait nft-trait .nft-interface)

;; Error codes
(define-constant ERR_NOT_OWNER u100)
(define-constant ERR_NOT_STAKED u101)
(define-constant ERR_STAKE_PERIOD u102)
(define-constant ERR_TRANSFER_FAIL u103)
(define-constant ERR_ALREADY_STAKED u104)

;; Config (REWARD_PER_BLOCK must be a data-var to change)
(define-constant MIN_STAKE_DURATION u100)
(define-data-var reward-per-block uint u100000)

;; Data storage
(define-map staked-by (uint) principal)
(define-map stake-start (uint) uint)
(define-map accumulated-rewards (uint) uint)
(define-data-var contract-owner principal tx-sender)

;; NFT + GIP Token contract references
(define-constant NFT-CONTRACT .nft-collection)
(define-constant GIP-TOKEN-CONTRACT .gip-token)

;; ========================
;; CORE STAKING FUNCTIONS
;; ========================

(define-public (stake (token-id uint))
  (let (
    (owner (unwrap! (contract-call? NFT-CONTRACT get-owner token-id) (err ERR_NOT_OWNER)))
  )
    (begin
      (asserts! (is-eq tx-sender owner) (err ERR_NOT_OWNER))
      (asserts! (is-none (map-get? staked-by token-id)) (err ERR_ALREADY_STAKED))
      (unwrap! (contract-call? NFT-CONTRACT transfer token-id tx-sender (as-contract tx-sender)) (err ERR_TRANSFER_FAIL))
      (map-set staked-by token-id tx-sender)
      (map-set stake-start token-id block-height)
      (map-set accumulated-rewards token-id u0)
      (ok true)
    )
  )
)

(define-private (calculate-rewards (token-id uint))
  (let (
    (start (unwrap! (map-get? stake-start token-id) (err ERR_NOT_STAKED)))
    (elapsed (unwrap! (contract-call? .time-utils blocks-since start) (err u500)))
    (rate (var-get reward-per-block))
    (reward (unwrap! (contract-call? .math-utils safe-mul elapsed rate) (err u501)))
  )
    (ok reward)
  )
)

(define-public (unstake (token-id uint))
  (let (
    (staker (unwrap! (map-get? staked-by token-id) (err ERR_NOT_STAKED)))
    (start (unwrap! (map-get? stake-start token-id) (err ERR_NOT_STAKED)))
  )
    (begin
      (asserts! (is-eq tx-sender staker) (err ERR_NOT_OWNER))
      (let ((elapsed (unwrap! (contract-call? .time-utils blocks-since start) (err u500))))
        (asserts! (>= elapsed MIN_STAKE_DURATION) (err ERR_STAKE_PERIOD))
      )
      (let ((rewards (unwrap! (calculate-rewards token-id) (err u502))))
        (map-set accumulated-rewards token-id rewards)
      )
      (unwrap! (contract-call? NFT-CONTRACT transfer token-id (as-contract tx-sender) tx-sender) (err ERR_TRANSFER_FAIL))
      (map-delete staked-by token-id)
      (map-delete stake-start token-id)
      (ok true)
    )
  )
)

;; ========================
;; REWARD MANAGEMENT
;; ========================

(define-public (claim-rewards (token-id uint))
  (let (
    (staker (unwrap! (map-get? staked-by token-id) (err ERR_NOT_STAKED)))
  )
    (begin
      (asserts! (is-eq tx-sender staker) (err ERR_NOT_OWNER))
      (let (
        (rewards (unwrap! (calculate-rewards token-id) (err u502)))
        (current (unwrap! (map-get? accumulated-rewards token-id) (err ERR_NOT_STAKED)))
      )
        (map-set accumulated-rewards token-id u0)
        (map-set stake-start token-id block-height)
        (try! (contract-call? GIP-TOKEN-CONTRACT mint-gip tx-sender rewards))
        (ok rewards)
      )
    )
  )
)

;; ========================
;; VIEW FUNCTIONS
;; ========================

(define-read-only (get-stake-status (token-id uint))
  (let (
    (start (map-get? stake-start token-id))
    (duration (if (is-some start)
                    (unwrap-panic (contract-call? .time-utils blocks-since (unwrap start)))
                    u0))
  )
    (ok {
      staker: (map-get? staked-by token-id),
      start: start,
      rewards: (map-get? accumulated-rewards token-id),
      duration: duration
    })
  )
)

(define-read-only (get-pending-rewards (token-id uint))
  (calculate-rewards token-id)
)

(define-read-only (is-staked (token-id uint))
  (is-some (map-get? staked-by token-id))
)

;; ========================
;; ADMIN FUNCTIONS
;; ========================

(define-public (set-reward-rate (rate uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_NOT_OWNER))
    (var-set reward-per-block rate)
    (ok true)
  )
)

