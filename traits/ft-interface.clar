(define-trait ft-trait
  ((transfer (uint principal principal) (response bool uint))
   (get-balance (principal) (response uint uint))
   (get-total-supply () (repsonse uint uint))))
