(use-trait ft-trait .trait-sip-010.sip-010-trait)

;; ERRS
(define-constant ERR-INVALID-BLOCK (err u5000))
(define-constant ERR-INSUFFICIENT_AMOUNT (err u5001))
(define-constant ERR-INSUFFICIENT_BALANCE (err u5002))
(define-constant ERR-TRANSFER-FAILED (err u5003))
(define-constant ERR-BURN-FAILED (err u5004))
(define-constant ERR-INVALID-TOKEN (err u5005))
(define-constant ERR-INVALID-TOKEN-LOCK (err u5007))
(define-constant ERR-LOCK_MISMATCH (err u5008))
(define-constant ERR-NOT-AUTHORIZED (err u5009))
(define-constant ERR-INVALID-AMOUNT (err u6001))
(define-constant ERR-INVALID-POOL-TOKEN (err u6002))
(define-constant ERR-INVALID-LOCK (err u6003))
(define-constant ERR-FAILED (err u6004))
(define-constant ERR-OUT-OF-BOUNDS (err u6005))
(define-constant ERR-VEST-PARAMS-NOT-SET (err u7000))
(define-constant ERR-ONLY-SINGLE-LOCK (err u7001))
(define-constant ERR-ONLY-VESTED-LOCK (err u7002))
(define-constant ERR-MAX-VESTING-ADDRESSES (err u7003))
(define-constant ERR-VESTING-STARTED (err u7004))
(define-constant ERR-INVALID-INDEX (err u7005))
(define-constant ERR-VESTING-NOT-FOUND (err u7006))
(define-constant ERR-REWARD-BLOCK-NOT-REACHED (err u7007))
(define-constant ERR-CANNOT-EXCEED-CLAIM-AMOUNT (err u7008))
(define-constant ERR-INVALID-PERCENTAGE (err u8000))
(define-constant ERR-EXCEEDS-LOCKED-AMOUNT (err u8001))

;; DATA MAPS AND VARS

;; set caller as contract owner
(define-data-var contract-owner principal tx-sender)

;; maps user-addr to lock-ids
(define-map users-token-locks
    { user-addr: principal }
    (list 200 uint) 
)

;; maps user-addr to vesting-lock-ids
(define-map vested-user-tokens
    { user-addr: principal, lock-id: uint }
    {
      last-claim-block: uint,
      claim-index: uint, 
      total-claimed: uint,
      total-amount: uint,
    }
)

;; maps user-addr to vesting-user-locks
(define-map vested-user-locks
    { user-addr: principal}
    (list 200 uint)
)

;; maps pool id of token pairs to tokenlocks
(define-map token-lock-map
  { lock-id: uint }
  {
    lock-block: uint, ;; the date the token was locked
    amount: uint, ;; the amount of tokens still locked
    unlock-block: uint, ;; the date the token can be withdrawn
    lock-owner: principal, ;; the lock owner
    withdrawer: principal, ;; the address to be withdrawn to
    locked-token: principal, ;; the address of the token locked
    token-vested: bool,
    unvest-blocks: (optional (list 200 {height: uint, percentage: uint})),
    total-addresses: (optional uint),
  }
)

;; nonce of token locks
(define-data-var lock-nonce uint u0)

;; define the locker parameters
(define-data-var stx-fee uint u1000000) ;; small stacks fee to prevent spams
(define-data-var secondary-fee-token principal .memegoatstx) ;; in this case memegoat
(define-data-var secondary-token-fee uint u100000) ;; option memegoat ~ 10,000 memegoat

;; management calls

(define-public (set-contract-owner (owner principal))
  (begin
    (try! (check-is-owner)) 
    (ok (var-set contract-owner owner))
  )
)

;; read-only calls

(define-read-only (get-user-token-locks (user-addr principal)) 
  (default-to (list) (map-get? users-token-locks {user-addr: user-addr}))
)

(define-read-only (get-contract-owner)
  (ok (var-get contract-owner))
)

(define-read-only (get-token-lock-by-id (lock-id uint))
  (ok (unwrap! (map-get? token-lock-map {lock-id: lock-id}) ERR-INVALID-LOCK))
)

(define-read-only (get-user-vesting-details (user-addr principal) (lock-id uint))
  (ok (unwrap! (map-get? vested-user-tokens {user-addr: user-addr, lock-id: lock-id}) ERR-VESTING-NOT-FOUND))
)

(define-read-only (get-vested-user-locks (user-addr principal)) 
  (default-to (list) (map-get? vested-user-locks {user-addr: user-addr}))
)

;; private calls

(define-private (check-is-owner)
  (ok (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED))
)

;; set secondary fee token
(define-public (set-secondary-fee-token (secondary-token-trait <ft-trait>)) 
  (begin 
    (try! (check-is-owner))
    (var-set secondary-fee-token (contract-of secondary-token-trait)) 
    (ok true)
  )
)

;; fees for locking tokens
(define-public (set-fees (stx-fee_ uint) (secondary-token-fee_ uint)) 
  (begin 
    (try! (check-is-owner))
    (var-set stx-fee stx-fee_)
    (var-set secondary-token-fee secondary-token-fee_)
    (ok true)
  )
)

(define-private (add-lock-id (lock-id uint) (user-addr principal))
  (begin
    (and 
      (is-none (index-of (get-user-token-locks user-addr) lock-id))
      (map-set users-token-locks {user-addr: user-addr} (unwrap! (as-max-len? (append (get-user-token-locks user-addr) lock-id) u200) ERR-FAILED))
    )
    (ok true)
  )
)

(define-private (remove-lock-id (index uint) (lock-id uint) (user-addr principal))
  (begin
    (let (
          (lock-ids (get-user-token-locks user-addr))
          (length (len lock-ids))
          (last-item (unwrap! (element-at lock-ids (- length u1)) ERR-OUT-OF-BOUNDS))
          (item-to-remove (unwrap! (element-at lock-ids index) ERR-OUT-OF-BOUNDS))
          (updated-lists-v1 (unwrap! (replace-at? lock-ids (- length u1) item-to-remove) ERR-OUT-OF-BOUNDS)) 
          (updated-lists-v2 (unwrap! (replace-at? updated-lists-v1 index last-item) ERR-OUT-OF-BOUNDS)) 
        )
        (map-set users-token-locks {user-addr: user-addr} (unwrap! (as-max-len? (unwrap-panic (slice? updated-lists-v2 u0 (- length u1))) u200) ERR-FAILED))
    )
    (ok true)
  )
)

(define-private (add-vested-lock-id (lock-id uint) (user-addr principal))
  (begin
    (and 
      (is-none (index-of (get-vested-user-locks user-addr) lock-id))
      (map-set vested-user-locks {user-addr: user-addr} (unwrap! (as-max-len? (append (get-vested-user-locks user-addr) lock-id) u200) ERR-FAILED))
    )
    (ok true)
  )
)

(define-private (store-vesting-record-iter (user-record {addr: principal, amount: uint}) (lock-id uint))
  (begin
    (unwrap-panic (add-vested-lock-id lock-id (get addr user-record)))
    (map-set vested-user-tokens {user-addr: (get addr user-record), lock-id: lock-id} {last-claim-block: u0, claim-index: u0, total-claimed: u0, total-amount: (get amount user-record)})
    lock-id
  )
)

(define-private (sum-unvest-blocks-percentage (unvest-blocks {height: uint, percentage: uint}) (total-percentage uint))
  (begin 
    (+ total-percentage (get percentage unvest-blocks))
  )
)

(define-private (sum-amount-vesting-record (user-record {addr: principal, amount: uint}) (amount uint))
  (begin 
    (+ amount (get amount user-record))
  )
)

;; lockToken
(define-public 
  (lock-token 
    (amount uint)
    (unlock-block uint) 
    (fee-in-stx bool) 
    (locked-token <ft-trait>) 
    (secondary-token-trait <ft-trait>) 
    (withdrawer principal)
    (is-vested-lock bool)
    (unvest-blocks (optional (list 200 {height: uint, percentage: uint})))
    (addresses (optional (list 200 {addr: principal, amount: uint})))
  ) 
  (begin     
    (asserts! (> unlock-block block-height) ERR-INVALID-BLOCK)
    (asserts! (> amount u0) ERR-INSUFFICIENT_AMOUNT)
  
    (let 
      (
        (stxfee (var-get stx-fee))
        (secondarytokenfee (var-get secondary-token-fee))
        (sender tx-sender)
        (next-lock-id (+ (var-get lock-nonce) u1))
        (locked-token_ (contract-of locked-token))
       
      )

      (if fee-in-stx
        ;; Pay fee in STX
        (try! (stx-transfer? stxfee tx-sender .memegoat-vault))
        ;; Burn token
        (begin
          (asserts! (is-eq (var-get secondary-fee-token) (contract-of secondary-token-trait)) ERR-INVALID-TOKEN)
          (try! (contract-call? secondary-token-trait burn-fixed secondarytokenfee sender))
        )
      )

      ;; transfer token to vault
      (try! (contract-call? locked-token transfer amount sender .memegoat-vault none))

      (if is-vested-lock
        (begin
          (let 
            (
              (unwrapped-unvest-blocks (unwrap! unvest-blocks ERR-VEST-PARAMS-NOT-SET))
              (unwrapped-addresses  (unwrap! addresses ERR-VEST-PARAMS-NOT-SET))
              (total-percentage (fold sum-unvest-blocks-percentage unwrapped-unvest-blocks u0))
              (amount-to-register (fold sum-amount-vesting-record unwrapped-addresses u0))
            )
            (asserts! (is-eq total-percentage u100) ERR-INVALID-PERCENTAGE)
            (asserts! (is-eq amount-to-register amount) ERR-EXCEEDS-LOCKED-AMOUNT)
            (fold store-vesting-record-iter unwrapped-addresses next-lock-id)
            (map-set token-lock-map 
              {lock-id: next-lock-id} 
              { 
                lock-block: block-height,
                amount: amount, 
                unlock-block: unlock-block, 
                lock-owner: sender, 
                locked-token: locked-token_, 
                withdrawer: withdrawer,
                token-vested: true,
                unvest-blocks: unvest-blocks,
                total-addresses: (some (len unwrapped-addresses)),
              }
            )
          )
        )
        (map-set token-lock-map 
          {lock-id: next-lock-id} 
          { 
            lock-block: block-height,
            amount: amount, 
            unlock-block: unlock-block, 
            lock-owner: sender, 
            locked-token: locked-token_, 
            withdrawer: withdrawer,
            token-vested: false,
            unvest-blocks: none,
            total-addresses: none,
          }
        )
      )
      ;; add lock id
      (try! (add-lock-id next-lock-id sender))
      ;; update lock nonce
      (var-set lock-nonce next-lock-id)
    )
    (ok true)  
  )
)

;; relockToken
(define-public (relock-token (index uint) (new-unlock-block uint)) 
  (begin 
    (let
      (
        (sender tx-sender)
        (lock-id (unwrap! (element-at (get-user-token-locks sender) index) ERR-OUT-OF-BOUNDS))
        (token-lock (unwrap! (map-get? token-lock-map { lock-id: lock-id }) ERR-INVALID-TOKEN-LOCK))
        (token-vested (get token-vested token-lock))
        (token-lock-updated (merge token-lock {
          unlock-block: new-unlock-block
        }))
      )

      (asserts! (not token-vested) ERR-ONLY-SINGLE-LOCK)
      (asserts! (and (> new-unlock-block (get lock-block token-lock)) (> new-unlock-block block-height)) ERR-INVALID-BLOCK)
      (asserts! (is-some (index-of (get-user-token-locks sender) lock-id)) ERR-OUT-OF-BOUNDS)
      (asserts! (is-eq (get lock-owner token-lock) sender) ERR-LOCK_MISMATCH)
      (map-set token-lock-map { lock-id: lock-id } token-lock-updated)
    )
    (ok true)
  )
)

;; withdraw
(define-public (withdraw-token (locked-token <ft-trait>) (is-vested-lock bool) (index uint)) 
  (begin
    (if is-vested-lock
      (begin
        (let
          (
            (sender tx-sender)
            (token-lock (unwrap! (map-get? token-lock-map { lock-id: index }) ERR-INVALID-TOKEN-LOCK))
            (token-vested (get token-vested token-lock))
            (unvest-blocks (unwrap! (get unvest-blocks token-lock) ERR-VEST-PARAMS-NOT-SET))
            (user-vesting-details (try! (get-user-vesting-details sender index)))
            (total-amount (get total-amount user-vesting-details))
            (user-claim-index (get claim-index user-vesting-details))
            (user-total-claimed (get total-claimed user-vesting-details))
            (user-total-amount (get total-amount user-vesting-details))
            (user-unvest-block (unwrap! (element-at? unvest-blocks user-claim-index) ERR-OUT-OF-BOUNDS))
            (height (get height user-unvest-block))
            (percentage (get percentage user-unvest-block))
            (unlock-amount (/ (* total-amount percentage) u100))
            (user-vest-details-updated (merge user-vesting-details {
                claim-index: (+ user-claim-index u1),
                last-claim-block: block-height,
                total-claimed: (+ user-total-claimed unlock-amount)
              })
            )
          )

          (asserts! token-vested ERR-ONLY-VESTED-LOCK)
          (asserts! (> height block-height) ERR-REWARD-BLOCK-NOT-REACHED)
          (asserts! (is-eq (get locked-token token-lock) (contract-of locked-token)) ERR-INVALID-TOKEN)
          (asserts! (< user-total-claimed user-total-amount) ERR-CANNOT-EXCEED-CLAIM-AMOUNT)
          ;; transfer token from vault
          (as-contract (try! (contract-call? .memegoat-vault-v1 transfer-ft locked-token unlock-amount sender)))
          (map-set vested-user-tokens {user-addr: sender, lock-id: index} user-vest-details-updated)
        )
      )
      (let
        (
          (sender tx-sender)
          (lock-id (unwrap! (element-at (get-user-token-locks sender) index) ERR-OUT-OF-BOUNDS))
          (token-lock (unwrap! (map-get? token-lock-map { lock-id: lock-id }) ERR-INVALID-TOKEN-LOCK))
          (locked-balance (get amount token-lock))
          (unlock-block (get unlock-block token-lock))
          (withdrawer (get withdrawer token-lock))
          (token-vested (get token-vested token-lock))
          (token-lock-updated (merge token-lock {
            amount: u0
          }))
        )

        (asserts! (>= block-height unlock-block) ERR-INVALID-BLOCK)
        (asserts! (is-eq (get lock-owner token-lock) sender) ERR-LOCK_MISMATCH)
        (asserts! (is-eq (get locked-token token-lock) (contract-of locked-token)) ERR-INVALID-TOKEN)
        (asserts! (not token-vested) ERR-ONLY-SINGLE-LOCK)
        (asserts! (is-some (index-of (get-user-token-locks sender) lock-id)) ERR-OUT-OF-BOUNDS)
        ;; transfer token from vault
        (as-contract (try! (contract-call? .memegoat-vault-v1 transfer-ft locked-token locked-balance withdrawer))) 
        (map-set token-lock-map { lock-id: lock-id} token-lock-updated)
      )    
    )
    (ok true)
  )
)

;; incrementlock
(define-public (increment-lock (locked-token <ft-trait>) (index uint) (amount uint)) 
  (begin 
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)

    (let
      (
        (sender tx-sender)
        (lock-id (unwrap! (element-at (get-user-token-locks sender) index) ERR-OUT-OF-BOUNDS))
        (token-lock (unwrap! (map-get? token-lock-map { lock-id: lock-id }) ERR-INVALID-TOKEN-LOCK))
        (locked-balance (get amount token-lock))
        (token-vested (get token-vested token-lock))
        (token-lock-updated (merge token-lock {
          amount: (+ locked-balance amount)
          })
        )
      )

      ;; check that caller is owner
      (asserts! (is-eq (get lock-owner token-lock) sender) ERR-LOCK_MISMATCH)
      (asserts! (is-eq (get locked-token token-lock) (contract-of locked-token)) ERR-INVALID-TOKEN)
      (asserts! (not token-vested) ERR-ONLY-SINGLE-LOCK)
      (asserts! (is-some (index-of (get-user-token-locks sender) lock-id)) ERR-OUT-OF-BOUNDS)

      ;; transfer token to vault
      (try! (contract-call? locked-token transfer amount sender .memegoat-vault none))
      (map-set token-lock-map { lock-id: lock-id} token-lock-updated)
    )
    (ok true)
  )
)

;; splitlock
(define-public (split-lock (index uint) (amount uint)) 
  (begin 
      (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (let
      (
        (sender tx-sender)
        (lock-id (unwrap! (element-at (get-user-token-locks sender) index) ERR-OUT-OF-BOUNDS))
        (token-lock (unwrap! (map-get? token-lock-map { lock-id: lock-id }) ERR-INVALID-TOKEN-LOCK))
        (locked-balance (get amount token-lock))
        (unlock-block (get unlock-block token-lock))
        (lock-block (get lock-block token-lock))
        (withdrawer (get withdrawer token-lock))
        (locked-token (get locked-token token-lock))
        (token-vested (get token-vested token-lock))
        (token-lock-updated (merge token-lock {
          amount: (if (<= locked-balance amount) u0 (- locked-balance amount))
        }))
        (next-lock-id (+ (var-get lock-nonce) u1))
      )

      (asserts! (is-eq (get lock-owner token-lock) sender) ERR-LOCK_MISMATCH)
      (asserts! (<= amount locked-balance) ERR-INVALID-AMOUNT)  
      (asserts! (not token-vested) ERR-ONLY-SINGLE-LOCK)
      (asserts! (is-some (index-of (get-user-token-locks sender) lock-id)) ERR-OUT-OF-BOUNDS)
      (map-set token-lock-map {lock-id: lock-id} token-lock-updated)

      ;; create new token lock record
      (map-set token-lock-map 
        {lock-id: next-lock-id} 
        { 
          lock-block: block-height, 
          amount: amount, 
          unlock-block: unlock-block, 
          lock-owner: sender, 
          locked-token: locked-token, 
          withdrawer: withdrawer,
          token-vested: false,
          unvest-blocks: none,
          total-addresses: none,
        }
      )

      ;; add lock id
      (try! (add-lock-id next-lock-id sender))
      ;; update lock nonce
      (var-set lock-nonce next-lock-id)
    )
    (ok true)
  )
)

;; transferlockownership
(define-public (transfer-lock-ownership (index uint) (new-owner principal) (new-withdrawer principal)) 
  (begin
      
    (let
      (
        (sender tx-sender)
        (lock-id (unwrap! (element-at (get-user-token-locks sender) index) ERR-OUT-OF-BOUNDS))
        (token-lock (unwrap! (map-get? token-lock-map { lock-id: lock-id }) ERR-INVALID-TOKEN-LOCK))
        (token-lock-updated (merge token-lock {
          lock-owner: new-owner,
          withdrawer: new-withdrawer
        }))
      )

      (asserts! (is-eq (get lock-owner token-lock) sender) ERR-LOCK_MISMATCH)
      (asserts! (is-some (index-of (get-user-token-locks sender) lock-id)) ERR-OUT-OF-BOUNDS)
      (map-set token-lock-map {lock-id: lock-id} token-lock-updated)

      ;; add lock id
      (try! (add-lock-id lock-id new-owner))
      (try! (remove-lock-id index lock-id sender))
    )
    (ok true)
  )
)
