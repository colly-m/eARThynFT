(define-trait governance-trait
  ((propose (string-ascii 256) (response uint uint))
   (vote (uint int) (response bool uint))
   (get-proposal (uint) (response { description: (string-ascii 256), votes: int } uint))))
