(define-trait nft-trait
  ((get-owner (uint) (response principal (optional uint)))
   (transfer (uint principal principal) (response bool uint))
   (get-token-uri (uint) (response (string-ascii 256) uint))))
