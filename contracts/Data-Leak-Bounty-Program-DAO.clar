(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-BOUNTY-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-CLAIMED (err u103))
(define-constant ERR-INSUFFICIENT-BALANCE (err u104))
(define-constant ERR-INVALID-STATUS (err u105))
(define-constant ERR-INVALID-THRESHOLD (err u106))
(define-constant ERR-ALREADY-VOTED (err u107))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u108))
(define-constant ERR-PROPOSAL-EXPIRED (err u109))
(define-constant ERR-PROPOSAL-EXECUTED (err u110))
(define-constant ERR-PAYOUT-TOO-EARLY (err u111))
(define-constant ERR-PAYOUT-EXPIRED (err u112))
(define-constant ERR-INVALID-TIER (err u113))
(define-constant ERR-DISPUTE-NOT-FOUND (err u114))
(define-constant ERR-DISPUTE-EXPIRED (err u115))
(define-constant ERR-DISPUTE-RESOLVED (err u116))
(define-constant ERR-MEDIATOR-NOT-FOUND (err u117))
(define-constant ERR-INVALID-DISPUTE-STATUS (err u118))

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

(define-data-var proposal-counter uint u0)
(define-data-var voting-threshold uint u3)
(define-data-var proposal-duration uint u1440)

(define-map governors
    principal
    bool
)
(define-map proposals
    uint
    {
        proposer: principal,
        action: (string-ascii 50),
        target: principal,
        amount: uint,
        description: (string-ascii 256),
        votes-for: uint,
        votes-against: uint,
        executed: bool,
        created-at: uint,
        expires-at: uint,
    }
)

(define-map proposal-votes
    {
        proposal-id: uint,
        voter: principal,
    }
    {
        vote: bool,
        timestamp: uint,
    }
)

(define-public (add-governor (new-governor principal))
    (begin
        (asserts! (default-to false (map-get? governors tx-sender))
            ERR-NOT-AUTHORIZED
        )
        (map-set governors new-governor true)
        (ok true)
    )
)

(define-public (remove-governor (governor principal))
    (begin
        (asserts! (default-to false (map-get? governors tx-sender))
            ERR-NOT-AUTHORIZED
        )
        (map-delete governors governor)
        (ok true)
    )
)

(define-public (create-proposal
        (action (string-ascii 50))
        (target principal)
        (amount uint)
        (description (string-ascii 256))
    )
    (let ((proposal-id (+ (var-get proposal-counter) u1)))
        (asserts! (default-to false (map-get? governors tx-sender))
            ERR-NOT-AUTHORIZED
        )
        (map-set proposals proposal-id {
            proposer: tx-sender,
            action: action,
            target: target,
            amount: amount,
            description: description,
            votes-for: u0,
            votes-against: u0,
            executed: false,
            created-at: stacks-block-height,
            expires-at: (+ stacks-block-height (var-get proposal-duration)),
        })
        (var-set proposal-counter proposal-id)
        (ok proposal-id)
    )
)

(define-public (vote-proposal
        (proposal-id uint)
        (vote bool)
    )
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND)))
        (asserts! (default-to false (map-get? governors tx-sender))
            ERR-NOT-AUTHORIZED
        )
        (asserts! (< stacks-block-height (get expires-at proposal))
            ERR-PROPOSAL-EXPIRED
        )
        (asserts! (not (get executed proposal)) ERR-PROPOSAL-EXECUTED)
        (asserts!
            (is-none (map-get? proposal-votes {
                proposal-id: proposal-id,
                voter: tx-sender,
            }))
            ERR-ALREADY-VOTED
        )
        (map-set proposal-votes {
            proposal-id: proposal-id,
            voter: tx-sender,
        } {
            vote: vote,
            timestamp: stacks-block-height,
        })
        (if vote
            (map-set proposals proposal-id
                (merge proposal { votes-for: (+ (get votes-for proposal) u1) })
            )
            (map-set proposals proposal-id
                (merge proposal { votes-against: (+ (get votes-against proposal) u1) })
            )
        )
        (ok true)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND)))
        (asserts! (default-to false (map-get? governors tx-sender))
            ERR-NOT-AUTHORIZED
        )
        (asserts! (not (get executed proposal)) ERR-PROPOSAL-EXECUTED)
        (asserts! (>= (get votes-for proposal) (var-get voting-threshold))
            ERR-NOT-AUTHORIZED
        )
        (map-set proposals proposal-id (merge proposal { executed: true }))
        (ok true)
    )
)

(define-read-only (get-proposal (proposal-id uint))
    (ok (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
)

(define-read-only (get-vote
        (proposal-id uint)
        (voter principal)
    )
    (ok (map-get? proposal-votes {
        proposal-id: proposal-id,
        voter: voter,
    }))
)

(define-read-only (is-governor (address principal))
    (ok (default-to false (map-get? governors address)))
)

(map-set governors tx-sender true)

(define-data-var auto-payout-delay uint u144)
(define-data-var escalation-period uint u1440)
(define-data-var max-auto-payout uint u10000)

(define-map payout-schedule
    uint
    {
        bounty-id: uint,
        scheduled-amount: uint,
        tier: (string-ascii 10),
        auto-release-height: uint,
        escalation-height: uint,
        claimed: bool,
        reporter: principal,
    }
)

(define-map tier-multipliers
    (string-ascii 10)
    {
        multiplier: uint,
        max-amount: uint,
    }
)

(define-public (initialize-tiers)
    (begin
        (map-set tier-multipliers "low" {
            multiplier: u100,
            max-amount: u1000,
        })
        (map-set tier-multipliers "medium" {
            multiplier: u200,
            max-amount: u5000,
        })
        (map-set tier-multipliers "high" {
            multiplier: u500,
            max-amount: u10000,
        })
        (map-set tier-multipliers "critical" {
            multiplier: u1000,
            max-amount: u50000,
        })
        (ok true)
    )
)

(define-public (schedule-payout
        (bounty-id uint)
        (base-amount uint)
        (tier (string-ascii 10))
        (reporter principal)
    )
    (let (
            (tier-info (unwrap! (map-get? tier-multipliers tier) ERR-INVALID-TIER))
            (calculated-amount (if (<= (* base-amount (get multiplier tier-info))
                    (get max-amount tier-info)
                )
                (* base-amount (get multiplier tier-info))
                (get max-amount tier-info)
            ))
        )
        (asserts! (<= calculated-amount (var-get max-auto-payout))
            ERR-INVALID-AMOUNT
        )
        (map-set payout-schedule bounty-id {
            bounty-id: bounty-id,
            scheduled-amount: calculated-amount,
            tier: tier,
            auto-release-height: (+ stacks-block-height (var-get auto-payout-delay)),
            escalation-height: (+ stacks-block-height (var-get escalation-period)),
            claimed: false,
            reporter: reporter,
        })
        (ok calculated-amount)
    )
)

(define-public (claim-auto-payout (bounty-id uint))
    (let ((payout (unwrap! (map-get? payout-schedule bounty-id) ERR-BOUNTY-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get reporter payout)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get claimed payout)) ERR-ALREADY-CLAIMED)
        (asserts! (>= stacks-block-height (get auto-release-height payout))
            ERR-PAYOUT-TOO-EARLY
        )
        (asserts! (< stacks-block-height (get escalation-height payout))
            ERR-PAYOUT-EXPIRED
        )
        (map-set payout-schedule bounty-id (merge payout { claimed: true }))
        (try! (as-contract (stx-transfer? (get scheduled-amount payout) tx-sender
            (get reporter payout)
        )))
        (ok (get scheduled-amount payout))
    )
)

(define-public (emergency-stop-payout (bounty-id uint))
    (let ((payout (unwrap! (map-get? payout-schedule bounty-id) ERR-BOUNTY-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get claimed payout)) ERR-ALREADY-CLAIMED)
        (map-delete payout-schedule bounty-id)
        (ok true)
    )
)

(define-public (escalate-payout (bounty-id uint))
    (let ((payout (unwrap! (map-get? payout-schedule bounty-id) ERR-BOUNTY-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get reporter payout)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get claimed payout)) ERR-ALREADY-CLAIMED)
        (asserts! (>= stacks-block-height (get escalation-height payout))
            ERR-PAYOUT-TOO-EARLY
        )
        (map-set payout-schedule bounty-id
            (merge payout {
                scheduled-amount: (+ (get scheduled-amount payout)
                    (/ (get scheduled-amount payout) u2)
                ),
                escalation-height: (+ stacks-block-height (var-get escalation-period)),
            })
        )
        (ok true)
    )
)

(define-public (set-auto-payout-delay (new-delay uint))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (var-set auto-payout-delay new-delay)
        (ok true)
    )
)

(define-public (set-max-auto-payout (new-max uint))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (var-set max-auto-payout new-max)
        (ok true)
    )
)

(define-read-only (get-payout-schedule (bounty-id uint))
    (ok (unwrap! (map-get? payout-schedule bounty-id) ERR-BOUNTY-NOT-FOUND))
)

(define-read-only (get-tier-info (tier (string-ascii 10)))
    (ok (unwrap! (map-get? tier-multipliers tier) ERR-INVALID-TIER))
)

(define-read-only (can-claim-now (bounty-id uint))
    (match (map-get? payout-schedule bounty-id)
        payout (ok (and
            (not (get claimed payout))
            (>= stacks-block-height (get auto-release-height payout))
            (< stacks-block-height (get escalation-height payout))
        ))
        (ok false)
    )
)

(define-map reputation-tiers
    (string-ascii 10)
    {
        min-score: uint,
        multiplier: uint,
    }
)

(define-public (initialize-reputation-tiers)
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (map-set reputation-tiers "bronze" {
            min-score: u0,
            multiplier: u100,
        })
        (map-set reputation-tiers "silver" {
            min-score: u50,
            multiplier: u125,
        })
        (map-set reputation-tiers "gold" {
            min-score: u150,
            multiplier: u150,
        })
        (map-set reputation-tiers "platinum" {
            min-score: u300,
            multiplier: u200,
        })
        (ok true)
    )
)

(define-read-only (get-reputation-multiplier (reporter principal))
    (let ((stats (default-to {
            total-reports: u0,
            total-earned: u0,
            reputation-score: u0,
        }
            (map-get? reporter-stats reporter)
        )))
        (let ((score (get reputation-score stats)))
            (if (>= score u300)
                u200
                (if (>= score u150)
                    u150
                    (if (>= score u50)
                        u125
                        u100
                    )
                )
            )
        )
    )
)

(define-read-only (calculate-reputation-bonus
        (base-amount uint)
        (reporter principal)
    )
    (let ((multiplier (get-reputation-multiplier reporter)))
        (/ (* base-amount multiplier) u100)
    )
)

(define-read-only (get-reporter-tier (reporter principal))
    (let ((stats (default-to {
            total-reports: u0,
            total-earned: u0,
            reputation-score: u0,
        }
            (map-get? reporter-stats reporter)
        )))
        (let ((score (get reputation-score stats)))
            (if (>= score u300)
                "platinum"
                (if (>= score u150)
                    "gold"
                    (if (>= score u50)
                        "silver"
                        "bronze"
                    )
                )
            )
        )
    )
)

(define-read-only (get-leaderboard-entry (reporter principal))
    (match (map-get? reporter-stats reporter)
        stats (ok {
            reporter: reporter,
            total-reports: (get total-reports stats),
            total-earned: (get total-earned stats),
            reputation-score: (get reputation-score stats),
            tier: (get-reporter-tier reporter),
            multiplier: (get-reputation-multiplier reporter),
        })
        ERR-NOT-AUTHORIZED
    )
)

(define-data-var dispute-counter uint u0)
(define-data-var dispute-period uint u2880)
(define-data-var mediator-fee uint u50)

(define-map disputes
    uint
    {
        bounty-id: uint,
        reporter: principal,
        reason: (string-ascii 256),
        status: (string-ascii 20),
        mediator: (optional principal),
        created-at: uint,
        resolved-at: uint,
        resolution: (string-ascii 256),
    }
)

(define-map mediators
    principal
    {
        total-resolved: uint,
        reputation-score: uint,
        is-active: bool,
    }
)

(define-map dispute-evidence
    {
        dispute-id: uint,
        submitter: principal,
    }
    {
        evidence: (string-ascii 500),
        timestamp: uint,
    }
)

(define-public (become-mediator)
    (begin
        (map-set mediators tx-sender {
            total-resolved: u0,
            reputation-score: u100,
            is-active: true,
        })
        (ok true)
    )
)

(define-public (deactivate-mediator (mediator-principal principal))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (match (map-get? mediators mediator-principal)
            mediator-stats (map-set mediators mediator-principal
                (merge mediator-stats { is-active: false })
            )
            false
        )
        (ok true)
    )
)

(define-public (create-dispute
        (bounty-id uint)
        (reason (string-ascii 256))
    )
    (let (
            (bounty (unwrap! (map-get? bounties bounty-id) ERR-BOUNTY-NOT-FOUND))
            (dispute-id (+ (var-get dispute-counter) u1))
        )
        (asserts! (is-eq tx-sender (get reporter bounty)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status bounty) "rejected") ERR-INVALID-STATUS)
        (map-set disputes dispute-id {
            bounty-id: bounty-id,
            reporter: tx-sender,
            reason: reason,
            status: "pending",
            mediator: none,
            created-at: stacks-block-height,
            resolved-at: u0,
            resolution: "",
        })
        (var-set dispute-counter dispute-id)
        (ok dispute-id)
    )
)

(define-public (assign-mediator
        (dispute-id uint)
        (mediator-principal principal)
    )
    (let ((dispute (unwrap! (map-get? disputes dispute-id) ERR-DISPUTE-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status dispute) "pending")
            ERR-INVALID-DISPUTE-STATUS
        )
        (match (map-get? mediators mediator-principal)
            mediator-stats (begin
                (asserts! (get is-active mediator-stats) ERR-MEDIATOR-NOT-FOUND)
                (map-set disputes dispute-id
                    (merge dispute {
                        mediator: (some mediator-principal),
                        status: "under-review",
                    })
                )
                (ok true)
            )
            ERR-MEDIATOR-NOT-FOUND
        )
    )
)

(define-public (submit-evidence
        (dispute-id uint)
        (evidence (string-ascii 500))
    )
    (let ((dispute (unwrap! (map-get? disputes dispute-id) ERR-DISPUTE-NOT-FOUND)))
        (asserts!
            (or
                (is-eq tx-sender (get reporter dispute))
                (is-eq tx-sender (var-get dao-owner))
            )
            ERR-NOT-AUTHORIZED
        )
        (asserts! (is-eq (get status dispute) "under-review")
            ERR-INVALID-DISPUTE-STATUS
        )
        (map-set dispute-evidence {
            dispute-id: dispute-id,
            submitter: tx-sender,
        } {
            evidence: evidence,
            timestamp: stacks-block-height,
        })
        (ok true)
    )
)

(define-public (resolve-dispute
        (dispute-id uint)
        (resolution (string-ascii 256))
        (approved bool)
        (award-amount uint)
    )
    (let ((dispute (unwrap! (map-get? disputes dispute-id) ERR-DISPUTE-NOT-FOUND)))
        (asserts!
            (match (get mediator dispute)
                mediator (is-eq tx-sender mediator)
                false
            )
            ERR-NOT-AUTHORIZED
        )
        (asserts! (is-eq (get status dispute) "under-review")
            ERR-INVALID-DISPUTE-STATUS
        )
        (asserts!
            (< stacks-block-height
                (+ (get created-at dispute) (var-get dispute-period))
            )
            ERR-DISPUTE-EXPIRED
        )
        (map-set disputes dispute-id
            (merge dispute {
                status: (if approved
                    "approved"
                    "rejected"
                ),
                resolved-at: stacks-block-height,
                resolution: resolution,
            })
        )
        (begin
            (if approved
                (let ((bounty (unwrap! (map-get? bounties (get bounty-id dispute))
                        ERR-BOUNTY-NOT-FOUND
                    )))
                    (map-set bounties (get bounty-id dispute)
                        (merge bounty {
                            status: "approved",
                            amount: award-amount,
                            claimed-at: stacks-block-height,
                        })
                    )
                    (try! (as-contract (stx-transfer? award-amount tx-sender (get reporter dispute))))
                    (var-set treasury-balance
                        (- (var-get treasury-balance) award-amount)
                    )
                    (match (map-get? reporter-stats (get reporter dispute))
                        prev-stats (map-set reporter-stats (get reporter dispute) {
                            total-reports: (get total-reports prev-stats),
                            total-earned: (+ (get total-earned prev-stats) award-amount),
                            reputation-score: (+ (get reputation-score prev-stats) u5),
                        })
                        (map-set reporter-stats (get reporter dispute) {
                            total-reports: u1,
                            total-earned: award-amount,
                            reputation-score: u5,
                        })
                    )
                )
                true
            )
            (match (get mediator dispute)
                mediator (match (map-get? mediators mediator)
                    mediator-stats (begin
                        (try! (as-contract (stx-transfer? (var-get mediator-fee) tx-sender mediator)))
                        (map-set mediators mediator
                            (merge mediator-stats {
                                total-resolved: (+ (get total-resolved mediator-stats) u1),
                                reputation-score: (+ (get reputation-score mediator-stats) u10),
                            })
                        )
                    )
                    true
                )
                true
            )
            (ok true)
        )
    )
)

(define-public (set-dispute-period (new-period uint))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (var-set dispute-period new-period)
        (ok true)
    )
)

(define-public (set-mediator-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (var-set mediator-fee new-fee)
        (ok true)
    )
)

(define-read-only (get-dispute (dispute-id uint))
    (ok (unwrap! (map-get? disputes dispute-id) ERR-DISPUTE-NOT-FOUND))
)

(define-read-only (get-mediator-stats (mediator-principal principal))
    (ok (unwrap! (map-get? mediators mediator-principal) ERR-MEDIATOR-NOT-FOUND))
)

(define-read-only (get-dispute-evidence
        (dispute-id uint)
        (submitter principal)
    )
    (ok (map-get? dispute-evidence {
        dispute-id: dispute-id,
        submitter: submitter,
    }))
)

(define-read-only (can-dispute (bounty-id uint))
    (match (map-get? bounties bounty-id)
        bounty (ok (is-eq (get status bounty) "rejected"))
        (ok false)
    )
)
