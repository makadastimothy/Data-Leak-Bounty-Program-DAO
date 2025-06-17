(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-BOUNTY-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-CLAIMED (err u103))
(define-constant ERR-INSUFFICIENT-BALANCE (err u104))
(define-constant ERR-INVALID-STATUS (err u105))

(define-data-var dao-owner principal tx-sender)
(define-data-var min-bounty-amount uint u1000)
(define-data-var total-bounties uint u0)
(define-data-var treasury-balance uint u0)

(define-map bounties
    uint
    {
        reporter: principal,
        amount: uint,
        description: (string-ascii 256),
        status: (string-ascii 20),
        created-at: uint,
        claimed-at: uint,
    }
)

(define-map reporter-stats
    principal
    {
        total-reports: uint,
        total-earned: uint,
        reputation-score: uint,
    }
)

(define-public (initialize-dao (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (var-set dao-owner new-owner)
        (ok true)
    )
)

(define-public (set-min-bounty-amount (new-amount uint))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (var-set min-bounty-amount new-amount)
        (ok true)
    )
)

(define-public (submit-vulnerability-report (description (string-ascii 256)))
    (let (
            (bounty-id (+ (var-get total-bounties) u1))
            (reporter-principal tx-sender)
        )
        (asserts! (>= (stx-get-balance tx-sender) (var-get min-bounty-amount))
            ERR-INSUFFICIENT-BALANCE
        )
        (try! (stx-transfer? (var-get min-bounty-amount) tx-sender
            (as-contract tx-sender)
        ))
        (var-set treasury-balance
            (+ (var-get treasury-balance) (var-get min-bounty-amount))
        )
        (map-set bounties bounty-id {
            reporter: reporter-principal,
            amount: (var-get min-bounty-amount),
            description: description,
            status: "pending",
            created-at: stacks-block-height,
            claimed-at: u0,
        })
        (match (map-get? reporter-stats reporter-principal)
            prev-stats (map-set reporter-stats reporter-principal {
                total-reports: (+ (get total-reports prev-stats) u1),
                total-earned: (get total-earned prev-stats),
                reputation-score: (get reputation-score prev-stats),
            })
            (map-set reporter-stats reporter-principal {
                total-reports: u1,
                total-earned: u0,
                reputation-score: u1,
            })
        )
        (var-set total-bounties bounty-id)
        (ok bounty-id)
    )
)

(define-public (approve-bounty
        (bounty-id uint)
        (reward-amount uint)
    )
    (let ((bounty (unwrap! (map-get? bounties bounty-id) ERR-BOUNTY-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status bounty) "pending") ERR-INVALID-STATUS)
        (asserts! (<= reward-amount (var-get treasury-balance))
            ERR-INSUFFICIENT-BALANCE
        )
        (map-set bounties bounty-id
            (merge bounty {
                amount: reward-amount,
                status: "approved",
                claimed-at: stacks-block-height,
            })
        )
        (try! (as-contract (stx-transfer? reward-amount tx-sender (get reporter bounty))))
        (var-set treasury-balance (- (var-get treasury-balance) reward-amount))
        (match (map-get? reporter-stats (get reporter bounty))
            prev-stats (map-set reporter-stats (get reporter bounty) {
                total-reports: (get total-reports prev-stats),
                total-earned: (+ (get total-earned prev-stats) reward-amount),
                reputation-score: (+ (get reputation-score prev-stats) u10),
            })
            (map-set reporter-stats (get reporter bounty) {
                total-reports: u1,
                total-earned: reward-amount,
                reputation-score: u10,
            })
        )
        (ok true)
    )
)

(define-public (reject-bounty (bounty-id uint))
    (let ((bounty (unwrap! (map-get? bounties bounty-id) ERR-BOUNTY-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status bounty) "pending") ERR-INVALID-STATUS)
        (map-set bounties bounty-id (merge bounty { status: "rejected" }))
        (ok true)
    )
)

(define-read-only (get-bounty (bounty-id uint))
    (ok (unwrap! (map-get? bounties bounty-id) ERR-BOUNTY-NOT-FOUND))
)

(define-read-only (get-reporter-stats (reporter principal))
    (ok (unwrap! (map-get? reporter-stats reporter) ERR-NOT-AUTHORIZED))
)

(define-read-only (get-treasury-balance)
    (ok (var-get treasury-balance))
)
