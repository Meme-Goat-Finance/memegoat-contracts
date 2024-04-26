(use-trait ft-trait .trait-sip-010.sip-010-trait)

(define-constant ERR-NOT-AUTHORIZED (err u1000))

(define-data-var contract-owner principal tx-sender)

;; read-only calls
(define-read-only (get-contract-owner)
  (ok (var-get contract-owner))
)

(define-public (get-meme-goat-balance)
  (begin 
    (contract-call? .memegoatstx get-balance-fixed (as-contract tx-sender))
  )
)

(define-public (get-testtSTX-balance)
  (begin 
    (contract-call? .testSTX get-balance-fixed (as-contract tx-sender))
  )
)

;; governance calls

(define-public (set-contract-owner (owner principal))
  (begin
    (try! (check-is-owner)) 
    (ok (var-set contract-owner owner))
  )
)

(define-public (get-faucet-tokens (dx uint) (dy uint) (recipient principal))
  (begin 
    (try! (transfer-meme-goat dx recipient))
    (transfer-testSTX dy recipient)
  )
)

;; private calls

(define-private (transfer-meme-goat (amount uint) (recipient principal))
  (begin     
    (as-contract (contract-call? .memegoatstx transfer-fixed amount tx-sender recipient none))
  )
)

(define-private (transfer-testSTX (amount uint) (recipient principal))
  (begin     
    (as-contract (contract-call? .testSTX transfer-fixed amount tx-sender recipient none))
  )
)

(define-private (check-is-owner)
  (ok (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED))
)

