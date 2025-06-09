;; ========================
;; IMPORTS
;; ========================
;; Assuming .math-utils and .time-utils are traits defined in local files
;; or if they are deployed contracts, you'd specify their full ID:
;; (use-trait .math-utils 'SP...contract-id.math-utils-trait)
;; (use-trait .time-utils 'SP...contract-id.time-utils-trait)
(use-trait math-utils-trait .math-utils.math-utils-trait)
(use-trait time-utils-trait .time-utils.time-utils-trait)
(use-trait nft-interface-trait .nft-interface.nft-interface-trait)

;; ========================
;; ERROR CODES
;; ========================
(define-constant ERR_NOT_OWNER u100)
(define-constant ERR_NOT_STAKED u101)
(define-constant ERR_STAKE_PERIOD u102)
(define-constant ERR_TRANSFER_FAIL u103)
(define-constant ERR_ALREADY_STAKED u104)
(define-constant ERR_TIME_UTILS_FAIL u105)         ;; For time-utils contract-call? errors (was u500)
(define-constant ERR_ARITHMETIC_OVERFLOW u106)     ;; For math-utils safe-mul/add/sub errors (was u501/u502)
(define-constant ERR_REWARDS_CALC_FAIL u107)       ;; For calculate-rewards unwrap (was u502)
(define-constant ERR_GIP_MINT_FAIL u108)           ;; For GIP token minting failures
(define-constant ERR_INVALID_REWARD_RATE u109)     ;; Added for set-reward-rate validation

;; ========================
;; CONFIG
;; ========================
(define-constant MIN_STAKE_DURATION u100)
(define-data-var reward-per-block uint u100000) ;; This can be changed by the owner

;; ========================
;; DATA STORAGE
;; ========================
(define-map staked-by (uint) principal)        ;; tokenId -> staker principal
(define-map stake-start (uint) uint)           ;; tokenId -> block-height start
(define-map accumulated-rewards (uint) uint)   ;; tokenId -> accumulated (unclaimed) rewards
(define-data-var contract-owner principal tx-sender)

;; NFT + GIP Token contract references
;; These should be the deployed contract IDs for your NFT collection and GIP token
;; Example: 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.nft-collection-v1'
(define-constant NFT-CONTRACT .nft-collection)
(define-constant GIP-TOKEN-CONTRACT .gip-token)


;; Core Staking Functions

(define-public (stake (token-id uint))
  (let (
    (owner (unwrap! (contract-call? NFT-CONTRACT get-owner token-id) (err ERR_NOT_OWNER)))
  )
    (begin
      ;; Ensure the transaction sender is the NFT owner
      (asserts! (is-eq tx-sender owner) (err ERR_NOT_OWNER))
      ;; Ensure the NFT is not already staked
      (asserts! (is-none (map-get? staked-by token-id)) (err ERR_ALREADY_STAKED))

      ;; Transfer NFT from tx-sender to this staking contract
      (try! (contract-call? NFT-CONTRACT transfer token-id tx-sender (as-contract tx-sender)))

      ;; Record stake details
      (map-set staked-by token-id tx-sender)
      (map-set stake-start token-id block-height)
      (map-set accumulated-rewards token-id u0) ;; Initialize accumulated rewards to 0
      (ok true)
    )
  )
)

(define-private (calculate-rewards (token-id uint))
  (let (
    (start-block (unwrap! (map-get? stake-start token-id) (err ERR_NOT_STAKED)))
    ;; Calculate elapsed blocks since staking
    (elapsed-result (contract-call? .time-utils blocks-since start-block))
  )
    (asserts! (is-ok elapsed-result) (err ERR_TIME_UTILS_FAIL))
    (let (
      (elapsed-blocks (unwrap! elapsed-result (err ERR_TIME_UTILS_FAIL)))
      (rate (var-get reward-per-block))
      ;; Calculate total reward based on elapsed blocks and rate
      (reward-result (contract-call? .math-utils safe-mul elapsed-blocks rate))
    )
      (asserts! (is-ok reward-result) (err ERR_ARITHMETIC_OVERFLOW))
      (ok (unwrap! reward-result (err ERR_ARITHMETIC_OVERFLOW)))
    )
  )
)

(define-public (unstake (token-id uint))
  (let (
    (staker (unwrap! (map-get? staked-by token-id) (err ERR_NOT_STAKED)))
    (start-block (unwrap! (map-get? stake-start token-id) (err ERR_NOT_STAKED)))
  )
    (begin
      ;; Ensure the transaction sender is the staker
      (asserts! (is-eq tx-sender staker) (err ERR_NOT_OWNER))

      ;; Check minimum stake duration
      (let ((elapsed-result (contract-call? .time-utils blocks-since start-block)))
        (asserts! (is-ok elapsed-result) (err ERR_TIME_UTILS_FAIL))
        (let ((elapsed-blocks (unwrap! elapsed-result (err ERR_TIME_UTILS_FAIL))))
          (asserts! (>= elapsed-blocks MIN_STAKE_DURATION) (err ERR_STAKE_PERIOD))
        )
      )

      ;; Calculate and set accumulated rewards (this is often done on claim, but kept for logic)
      (let ((rewards-to-accrue (unwrap! (calculate-rewards token-id) (err ERR_REWARDS_CALC_FAIL))))
        ;; Add to existing accumulated, don't overwrite if partial claims occurred
        (map-set accumulated-rewards token-id (+ (default-to u0 (map-get? accumulated-rewards token-id)) rewards-to-accrue))
      )

      ;; Transfer NFT from this staking contract back to tx-sender
      (try! (contract-call? NFT-CONTRACT transfer token-id (as-contract tx-sender) tx-sender))

      ;; Clear stake data
      (map-delete staked-by token-id)
      (map-delete stake-start token-id)
      ;; Note: Accumulated rewards are kept until claimed, but the stake is removed
      (ok true)
    )
  )
)


;; Reward Management

(define-public (claim-rewards (token-id uint))
  (let (
    (staker (unwrap! (map-get? staked-by token-id) (err ERR_NOT_STAKED)))
    (current-rewards (unwrap! (map-get? accumulated-rewards token-id) (err ERR_NOT_STAKED)))
  )
    (begin
      ;; Ensure the transaction sender is the staker
      (asserts! (is-eq tx-sender staker) (err ERR_NOT_OWNER))

      ;; Calculate new rewards since last claim/stake
      (let ((new-rewards (unwrap! (calculate-rewards token-id) (err ERR_REWARDS_CALC_FAIL))))
        (let ((total-claimable-rewards (+ current-rewards new-rewards)))
          (asserts! (> total-claimable-rewards u0) (err ERR_REWARDS_CALC_FAIL)) ;; Ensure there are rewards to claim

          ;; Reset accumulated rewards and update stake start for next period
          (map-set accumulated-rewards token-id u0)
          (map-set stake-start token-id block-height) ;; Reset stake-start for continuous staking

          ;; Mint GIP tokens to the staker
          ;; CRITICAL FIX: Changed 'mint-gip' to 'mint' based on your GIP token contract
          (try! (contract-call? GIP-TOKEN-CONTRACT mint tx-sender total-claimable-rewards))
          (ok total-claimable-rewards)
        )
      )
    )
  )
)


;; View Functions

(define-read-only (get-stake-status (token-id uint))
  (let (
    (staker-option (map-get? staked-by token-id))
    (start-block-option (map-get? stake-start token-id))
    (current-rewards-option (map-get? accumulated-rewards token-id))
    (current-duration u0)
  )
    ;; Calculate duration only if staked
    (if (is-some start-block-option)
      (let ((elapsed-result (contract-call? .time-utils blocks-since (unwrap! start-block-option (err u0))))) ;; Default error is fine here
        (if (is-ok elapsed-result)
          (var-set current-duration (unwrap! elapsed-result (err u0)))
          (var-set current-duration u0) ;; Set to 0 if time-utils call fails
        )
      )
    )
    (ok {
      staker: staker-option,
      start: start-block-option,
      rewards: current-rewards-option,
      duration: current-duration
    })
  )
)

(define-read-only (get-pending-rewards (token-id uint))
  ;; This function now adds newly calculated rewards to any existing accumulated rewards
  ;; It doesn't reset the stake-start or accumulated rewards, only calculates the *current* pending amount
  (let (
    (new-rewards (default-to u0 (unwrap! (calculate-rewards token-id) (err u0)))) ;; Default to u0 if not staked or error
    (accumulated (default-to u0 (map-get? accumulated-rewards token-id)))
  )
    (ok (+ new-rewards accumulated))
  )
)

(define-read-only (is-staked (token-id uint))
  (is-some (map-get? staked-by token-id))
)


;; Admin Functions

(define-public (set-reward-rate (rate uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_NOT_OWNER))
    (asserts! (> rate u0) (err ERR_INVALID_REWARD_RATE)) ;; Ensure a positive reward rate
    (var-set reward-per-block rate)
    (ok true)
  )
)

(define-public (set-contract-owner (new-owner principal))
  ;; Allows transfer of ownership, e.g., to a DAO contract
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_NOT_OWNER))
    (var-set contract-owner new-owner)
    (ok true)
  )
)
