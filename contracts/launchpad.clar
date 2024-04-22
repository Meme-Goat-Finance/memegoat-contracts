(use-trait ft-trait .trait-sip-010.sip-010-trait)

;; ERRS
  
(define-constant ERR-INSUFFICIENT-AMOUNT (err u5001))
(define-constant ERR-NOT-AUTHORIZED (err u5009))
(define-constant ERR-PRESALE-STARTED (err u7000))
(define-constant ERR-PRESALE-NOT-STARTED (err u7001))
(define-constant ERR-PRESALE-NOT-ENDED (err u7002))
(define-constant ERR-NOT-PARTICIPANT (err u7003))
(define-constant ERR-POOL-NOT-FUNDED (err u8000))
(define-constant ERR-MAX-DEPOSIT-EXCEEDED (err u8001))

;; DATA MAPS AND VARS

;; set caller as contract owner
(define-data-var contract-owner principal tx-sender)

;; amount allocated for presale
(define-constant MEMEGOAT-POOL u1350000000000000)

(define-data-var stx-pool uint u0)
(define-data-var min-stx-deposit uint u20000000) ;; 20 STX
(define-data-var max-stx-deposit uint u200000000) ;; 200 STX
(define-data-var presale-started bool false)
(define-data-var no-of-participants uint u0)
(define-data-var duration uint u0)
(define-data-var release-block uint u0)

(define-map users-deposits
    { user-addr: principal }
    uint
)

;; MANAGEMENT CALLS

(define-public (set-contract-owner (owner principal))
  (begin
    (try! (check-is-owner)) 
    (ok (var-set contract-owner owner))
  )
)

(define-public (set-duration (no-of-blocks uint))
  (begin
    (try! (check-is-owner))
    (asserts! (not (var-get presale-started)) ERR-PRESALE-STARTED)
    (asserts! (>= no-of-blocks 144)) ;; rough estimate of one day
    (ok (var-set duration no-of-blocks))
  )
)

(define-public (start-presale)
  (begin
    (try! (check-is-owner))
    (asserts! (is-eq MEMEGOAT-POOL (try! (contract-call? .memegoatstx get-balance (as-contract tx-sender)))) ERR-POOL-NOT-FUNDED)
    (var-set presale-started true)
    (ok (var-set release-block (+ (var-get duration) block-height)))
  )
)

(define-public (fund-memegoat-launchpad (amount uint)))
  (begin
    (try! (check-is-owner))
    (asserts! (is-eq amount MEMEGOAT-POOL) ERR-INSUFFICIENT-AMOUNT)
    (asserts! (not (var-get presale-started)) ERR-PRESALE-STARTED)
    (try! (contract-call? .memegoatstx transfer amount tx-sender .launchpad-vault)
    (ok true)
)

;; READ ONLY CALLS

(define-read-only (get-user-deposits (user-addr principal)) 
  (default-to u0 (map-get? users-deposits {user-addr: user-ddr}))
)

(define-read-only (get-contract-owner)
  (ok (var-get contract-owner))
)

;; PRIVATE CALLS

(define-private (check-is-owner)
  (ok (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED))
)

(define-private (calculate-allocation (user-addr principal)))
    (let
      (
         user-deposit (get-user-deposits tx-sender)
         stx-pool-balance (var-get stx-pool)
     )
     (/ (* user-deposit MEMEGOAT-POOL) stx-pool-balance)
)

;; PUBLIC CALLS

;; depositStx
(define-public (deposit-stx (amount uint)))
  (begin
     (asserts! (>= amount (var-get min-stx-deposit)) ERR-INSUFFICIENT-AMOUNT)
     (asserts! (var-get presale-started) ERR-PRESALE-NOT-STARTED)
     (let
       (
         stx-pool-balance (var-get stx-pool)
         exists (is-some (map-get? users-deposits {user-addr: user-ddr}))
         user-deposit (get-user-deposits tx-sender)
         participants (var-get no-of-participants)
       )

      ;; check that user has not exceeded max deposit
      (asserts! (<= (+ user-deposit amount) (var-get max-stx-deposit)) ERR-MAX-DEPOSIT-EXCEEDED)
    
      ;; transfer stx to vault
      (try! (stx-transfer? amount tx-sender .launchpad-vault))

      ;; increment pool balance
      (var-set stx-pool (+ stx-pool-balance amount))

      ;; update user deposits
      (map-set users-deposits {user-addr:tx-sender} (+ user-deposit amount))

      ;; update no of participants
      ( if exists
         (var-set no-of-participants participants)
         (var-set no-of-participants (+ participants u1)
       )
    )
    (ok true)
)

;; claim memegoat
(define-public (claim-memegoat))
  (begin
     (asserts! (var-get presale-started) ERR-PRESALE-NOT-STARTED)
     (asserts! (< (var-get release-block) block-height) ERR-PRESALE-NOT-ENDED)
     (let
        (
           exists (is-some (map-get? users-deposits {user-addr: user-ddr}))
           user-allocation (calculate-allocation tx-sender))
        )

        (asserts! exists ERR-NOT-PARTICIPANT)
      
        ;; transfer token from vault
       (as-contract (try! (contract-call? .launchpad-vault transfer-memegoat amount tx-sender))) 
    )
    (ok true)
)
