(define-public (test-full-flow)
  ;; Mint NFT → Record impact → Mint GIP → Stake NFT
  (let ((user tx-sender))
    (let ((mint-result (contract-call? .nft-collection mint user)))
      (asserts! (is-ok mint-result) "Minting should work")
      (let ((token-id (unwrap! mint-result u0)))

        (let ((impact-result (contract-call? .impact-tracker record-impact token-id 50)))
          (asserts! (is-ok impact-result) "Impact should record"))

        (let ((gip-result (contract-call? .gip-token mint-gip user u50)))
          (asserts! (is-ok gip-result) "GIP minting should work"))

        (let ((stake-result (contract-call? .staking stake token-id)))
          (asserts! (is-ok stake-result) "Staking should succeed"))))))
