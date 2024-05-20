(use-trait ft-trait .trait-sip-010.sip-010-trait)

;; ERRS

(define-constant ERR-INSUFFICIENT-AMOUNT (err u5001))
(define-constant ERR-ZERO-AMOUNT (err u5002))
(define-constant ERR-INVALID-AMOUNT (err u5003))
(define-constant ERR-INVALID-ID (err u5004))
(define-constant ERR-NOT-AUTHORIZED (err u5009))
(define-constant ERR-BELOW-MIN-PERIOD (err u6000))
(define-constant ERR-LAUNCHPAD-INACTIVE (err u7000))
(define-constant ERR-INVALID-TOKEN (err u7001))
(define-constant ERR-TOKEN-LAUNCH-NOT-APPROVED (err u7000))
(define-constant ERR-TOKEN-LAUNCH-NOT-ENDED (err u7003))
(define-constant ERR-NOT-PARTICIPANT (err u7004))
(define-constant ERR-ALREADY-CLAIMED (err u7005))
(define-constant ERR-MAX-DEPOSIT-EXCEEDED (err u8001))
(define-constant ERR-HARDCAP-EXCEEDED (err u8002))
(define-constant ERR-MIN-TARGET-NOT-REACHED (err u8003))

;; DATA MAPS AND VARS

;; set caller as contract owner
(define-data-var contract-owner principal tx-sender)

;; nonce of launchpad
(define-data-var launchpad-nonce uint u0)

(define-map launchpad-map
  {token-launch-id: uint}
  {
    approved: bool,
    token: principal,
    pool-amount: uint,
    hardcap: uint,
    softcap: uint,
    total-stx-deposited: uint,
    no-of-participants: uint,
    min-stx-deposit: uint,
    max-stx-deposit: uint,
    duration: uint,
    start-block: uint,
    end-block: uint,
    owner: principal
  }
)

(define-data-var paused bool false)

(define-data-var launchpad-fee uint u500000000)

(define-map users-deposits
    { user-addr: principal, token-launch-id: uint }
    uint
)

(define-map user-claimed 
  { user-addr : principal, token-launch-id: uint }
  bool
)

;; MANAGEMENT CALLS

(define-public (set-contract-owner (owner principal))
  (begin
    (try! (check-is-owner)) 
    (ok (var-set contract-owner owner))
  )
)

(define-public (set-launchpad-fee (new-fee uint))
  (begin
    (try! (check-is-owner)) 
    (ok (var-set launchpad-fee new-fee))
  )
)

(define-public (approve-token-launch (token-launch-id uint))
  (begin
    (try! (check-is-owner))
    (let
      (
        (token-launch (try! (get-token-launch-by-id token-launch-id)))
        (duration (get duration token-launch))
        (token-launch-updated (merge token-launch {
            approved: true,
            start-block: block-height,
            end-block: (+ block-height duration)
          }
        ))
      )
      (map-set launchpad-map {token-launch-id: token-launch-id} token-launch-updated)
    )
    (ok true)
  )
)

;; READ ONLY CALLS

(define-read-only (get-token-launch-by-id (token-launch-id uint))
  (ok (unwrap! (map-get? launchpad-map {token-launch-id: token-launch-id}) ERR-INVALID-ID))
)

(define-read-only (get-user-deposits-exists (user-addr principal) (token-launch-id uint))
  (map-get? users-deposits {user-addr: user-addr, token-launch-id: token-launch-id})
)

(define-read-only (get-user-deposits (user-addr principal) (token-launch-id uint)) 
  (default-to u0 (get-user-deposits-exists user-addr token-launch-id))
)

(define-read-only (calculate-allocation (user-addr principal) (token-launch-id uint))
  (let
    ((user-deposit (get-user-deposits user-addr token-launch-id)))
    (* (unwrap-panic (get-stx-quote token-launch-id)) user-deposit) 
  )
)

(define-read-only (check-if-claimed (user-addr principal) (token-launch-id uint)) 
  (default-to false (map-get? user-claimed { user-addr: user-addr, token-launch-id: token-launch-id}))
)

(define-read-only (get-contract-owner)
  (ok (var-get contract-owner))
)

(define-read-only (get-stx-quote (token-launch-id uint))
  (let
    (
      (token-launch (try! (get-token-launch-by-id token-launch-id)))
      (token-pool (get pool-amount token-launch))
      (stx-pool (get total-stx-deposited token-launch))
    )

    (ok (/ token-pool stx-pool))
  )
)

(define-read-only (get-launchpad-fee)
  (var-get launchpad-fee)
)

;; PRIVATE CALLS

(define-private (check-is-owner)
  (ok (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED))
)

;; PUBLIC CALLS

(define-public 
  (register-token-launch 
    (token <ft-trait>)
    (pool-amount uint)
    (hardcap uint)
    (softcap uint)
    (duration uint)
    (min-stx-deposit uint)
    (max-stx-deposit uint)
  )
  (begin

    (asserts! (not (var-get paused)) ERR-LAUNCHPAD-INACTIVE)
    (asserts! (>= duration u144) ERR-BELOW-MIN-PERIOD) ;; rough estimate of one day
    (asserts! (> hardcap softcap) ERR-INVALID-AMOUNT)
    (asserts! (> pool-amount u0) ERR-ZERO-AMOUNT)
    (asserts! (or (> min-stx-deposit u0) (> max-stx-deposit u0)) ERR-ZERO-AMOUNT)

    (map-set launchpad-map {token-launch-id: (var-get launchpad-nonce)} {
      approved: false,
      token: (contract-of token),
      pool-amount: pool-amount,
      hardcap: (+ hardcap (get-launchpad-fee)),
      softcap: (+ softcap (get-launchpad-fee)),
      total-stx-deposited: u0,
      no-of-participants: u0,
      min-stx-deposit: min-stx-deposit,
      max-stx-deposit: max-stx-deposit,
      duration: duration,
      start-block: u0,
      end-block: u0,
      owner: tx-sender
    })

    (try! (contract-call? token transfer pool-amount tx-sender .memegoat-vault-v1 none))

    (ok true)
  )
)

;; depositStx
(define-public (deposit-stx (amount uint) (token-launch-id uint))
  (begin
    (asserts! (not (var-get paused)) ERR-LAUNCHPAD-INACTIVE)

    (let
      (
        (sender tx-sender)
        (token-launch (try! (get-token-launch-by-id token-launch-id)))
        (total-stx-deposited (get total-stx-deposited token-launch))
        (participants (get no-of-participants token-launch))
        (min-stx-deposit (get min-stx-deposit token-launch))
        (max-stx-deposit (get max-stx-deposit token-launch))
        (exists (is-some (get-user-deposits-exists sender token-launch-id)))
        (user-deposit (get-user-deposits sender token-launch-id))
        (end-block (get end-block token-launch))
        (approved (get approved token-launch))
        (hardcap (get hardcap token-launch))

        (token-launch-updated (merge token-launch {
          total-stx-deposited: (+ total-stx-deposited amount),
          no-of-participants: (if exists participants (+ participants u1))
          }
        ))
      )
      
      ;; check that that token launch has been approved
      (asserts! approved ERR-TOKEN-LAUNCH-NOT-APPROVED)

      (asserts! (>= amount min-stx-deposit) ERR-INSUFFICIENT-AMOUNT)

      ;; check that hardcap has not been reached
      (asserts! (<= (+ amount total-stx-deposited) hardcap) ERR-HARDCAP-EXCEEDED)

      ;; check that user has not exceeded max deposit
      (asserts! (<= (+ user-deposit amount)  max-stx-deposit) ERR-MAX-DEPOSIT-EXCEEDED)
    
      ;; transfer stx to vault
      (try! (stx-transfer? amount tx-sender .memegoat-vault-v1))

      ;; updated user-deposits
      (map-set users-deposits {user-addr: sender, token-launch-id: token-launch-id} (+ user-deposit amount))

    )
    (ok true)
  )
)

;; claim memegoat
(define-public (claim-token (token-launch-id uint) (token-trait <ft-trait>))
  (begin
    (asserts! (not (var-get paused)) ERR-LAUNCHPAD-INACTIVE)
    (let
      (
        (sender tx-sender)
        (token-launch (try! (get-token-launch-by-id token-launch-id)))
        (total-stx-deposited (get total-stx-deposited token-launch))
        (softcap (get softcap token-launch))
        (hardcap (get softcap token-launch))
        (token (get token token-launch))
        (end-block (get end-block token-launch))
        (exists (is-some (get-user-deposits-exists sender token-launch-id)))
        (user-allocation (calculate-allocation sender token-launch-id))
        (claimed (check-if-claimed sender token-launch-id))
      )

      (asserts! (>= total-stx-deposited softcap) ERR-MIN-TARGET-NOT-REACHED)
      (asserts! (or (< end-block block-height) (is-eq total-stx-deposited hardcap)) ERR-TOKEN-LAUNCH-NOT-ENDED)
      (asserts! exists ERR-NOT-PARTICIPANT)
      (asserts! (not claimed) ERR-ALREADY-CLAIMED)
      (asserts! (is-eq token (contract-of token-trait)) ERR-INVALID-TOKEN)
          
      ;; transfer token from vault
      (as-contract (try! (contract-call? .memegoat-vault-v1 transfer-ft token-trait user-allocation sender)))      
      
      ;; set user status to claimed 
      (map-set user-claimed { user-addr: sender, token-launch-id: token-launch-id } true)
    )
    (ok true)
  )
)

