(define-trait whitelist-trait
  ((add-to-whitelist (principal) (response bool uint))
   (is-whitelisted (principal) (response bool uint))))
