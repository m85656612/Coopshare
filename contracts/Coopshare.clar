(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_ALREADY_MEMBER (err u101))
(define-constant ERR_NOT_MEMBER (err u102))
(define-constant ERR_INSUFFICIENT_SHARES (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u105))
(define-constant ERR_ALREADY_VOTED (err u106))
(define-constant ERR_VOTING_ENDED (err u107))
(define-constant ERR_VOTING_ACTIVE (err u108))
(define-constant ERR_INSUFFICIENT_FUNDS (err u109))
(define-constant ERR_NO_DIVIDENDS (err u110))

(define-data-var total-shares uint u0)
(define-data-var total-members uint u0)
(define-data-var proposal-counter uint u0)
(define-data-var dividend-pool uint u0)
(define-data-var last-dividend-block uint u0)

(define-map members principal {
    shares: uint,
    joined-at: uint,
    last-dividend-claim: uint
})

(define-map proposals uint {
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposer: principal,
    created-at: uint,
    voting-ends: uint,
    yes-votes: uint,
    no-votes: uint,
    executed: bool
})

(define-map votes {proposal-id: uint, voter: principal} bool)

(define-map share-transfers {from: principal, to: principal, amount: uint} uint)

(define-public (join-coop (initial-shares uint))
    (let (
        (caller tx-sender)
        (current-block stacks-block-height)
    )
    (asserts! (> initial-shares u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? members caller)) ERR_ALREADY_MEMBER)
    
    (map-set members caller {
        shares: initial-shares,
        joined-at: current-block,
        last-dividend-claim: current-block
    })
    
    (var-set total-shares (+ (var-get total-shares) initial-shares))
    (var-set total-members (+ (var-get total-members) u1))
    
    (ok true)
    )
)

(define-public (purchase-shares (amount uint))
    (let (
        (caller tx-sender)
        (member-data (unwrap! (map-get? members caller) ERR_NOT_MEMBER))
        (share-price u1000000)
        (total-cost (* amount share-price))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (stx-get-balance caller) total-cost) ERR_INSUFFICIENT_FUNDS)
    
    (try! (stx-transfer? total-cost caller (as-contract tx-sender)))
    
    (map-set members caller (merge member-data {
        shares: (+ (get shares member-data) amount)
    }))
    
    (var-set total-shares (+ (var-get total-shares) amount))
    (var-set dividend-pool (+ (var-get dividend-pool) total-cost))
    
    (ok amount)
    )
)

(define-public (transfer-shares (to principal) (amount uint))
    (let (
        (caller tx-sender)
        (from-member (unwrap! (map-get? members caller) ERR_NOT_MEMBER))
        (to-member (unwrap! (map-get? members to) ERR_NOT_MEMBER))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (get shares from-member) amount) ERR_INSUFFICIENT_SHARES)
    
    (map-set members caller (merge from-member {
        shares: (- (get shares from-member) amount)
    }))
    
    (map-set members to (merge to-member {
        shares: (+ (get shares to-member) amount)
    }))
    
    (ok true)
    )
)

(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)) (voting-duration uint))
    (let (
        (caller tx-sender)
        (proposal-id (+ (var-get proposal-counter) u1))
        (current-block stacks-block-height)
    )
    (asserts! (is-some (map-get? members caller)) ERR_NOT_MEMBER)
    (asserts! (> voting-duration u0) ERR_INVALID_AMOUNT)
    
    (map-set proposals proposal-id {
        title: title,
        description: description,
        proposer: caller,
        created-at: current-block,
        voting-ends: (+ current-block voting-duration),
        yes-votes: u0,
        no-votes: u0,
        executed: false
    })
    
    (var-set proposal-counter proposal-id)
    
    (ok proposal-id)
    )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-yes bool))
    (let (
        (caller tx-sender)
        (member-data (unwrap! (map-get? members caller) ERR_NOT_MEMBER))
        (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
        (current-block stacks-block-height)
        (vote-key {proposal-id: proposal-id, voter: caller})
        (member-shares (get shares member-data))
    )
    (asserts! (< current-block (get voting-ends proposal)) ERR_VOTING_ENDED)
    (asserts! (is-none (map-get? votes vote-key)) ERR_ALREADY_VOTED)
    
    (map-set votes vote-key true)
    
    (if vote-yes
        (map-set proposals proposal-id (merge proposal {
            yes-votes: (+ (get yes-votes proposal) member-shares)
        }))
        (map-set proposals proposal-id (merge proposal {
            no-votes: (+ (get no-votes proposal) member-shares)
        }))
    )
    
    (ok true)
    )
)

(define-public (distribute-dividends)
    (let (
        (caller tx-sender)
        (current-block stacks-block-height)
        (pool-amount (var-get dividend-pool))
    )
    (asserts! (is-eq caller CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> pool-amount u0) ERR_NO_DIVIDENDS)
    
    (var-set last-dividend-block current-block)
    (var-set dividend-pool u0)
    
    (ok pool-amount)
    )
)

(define-public (claim-dividend)
    (let (
        (caller tx-sender)
        (member-data (unwrap! (map-get? members caller) ERR_NOT_MEMBER))
        (member-shares (get shares member-data))
        (total-pool (var-get dividend-pool))
        (total-supply (var-get total-shares))
        (dividend-amount (/ (* total-pool member-shares) total-supply))
        (current-block stacks-block-height)
    )
    (asserts! (> dividend-amount u0) ERR_NO_DIVIDENDS)
    (asserts! (> current-block (get last-dividend-claim member-data)) ERR_NO_DIVIDENDS)
    
    (try! (as-contract (stx-transfer? dividend-amount tx-sender caller)))
    
    (map-set members caller (merge member-data {
        last-dividend-claim: current-block
    }))
    
    (ok dividend-amount)
    )
)

(define-public (add-to-dividend-pool)
    (let (
        (amount (stx-get-balance tx-sender))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set dividend-pool (+ (var-get dividend-pool) amount))
    
    (ok amount)
    )
)

(define-read-only (get-member-info (member principal))
    (map-get? members member)
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (get-total-shares)
    (ok (var-get total-shares))
)

(define-read-only (get-total-members)
    (ok (var-get total-members))
)

(define-read-only (get-dividend-pool)
    (ok (var-get dividend-pool))
)

(define-read-only (get-proposal-count)
    (ok (var-get proposal-counter))
)

(define-read-only (calculate-voting-power (member principal))
    (match (map-get? members member)
        member-data (ok (get shares member-data))
        ERR_NOT_MEMBER
    )
)

(define-read-only (get-proposal-status (proposal-id uint))
    (match (map-get? proposals proposal-id)
        proposal (let (
            (current-block stacks-block-height)
            (voting-ended (>= current-block (get voting-ends proposal)))
            (yes-votes (get yes-votes proposal))
            (no-votes (get no-votes proposal))
            (passed (> yes-votes no-votes))
        )
        (ok {
            voting-ended: voting-ended,
            passed: passed,
            yes-votes: yes-votes,
            no-votes: no-votes,
            total-votes: (+ yes-votes no-votes)
        }))
        ERR_PROPOSAL_NOT_FOUND
    )
)

(define-read-only (get-member-dividend-info (member principal))
    (match (map-get? members member)
        member-data (let (
            (member-shares (get shares member-data))
            (total-supply (var-get total-shares))
            (pool-amount (var-get dividend-pool))
            (estimated-dividend (if (> total-supply u0)
                (/ (* pool-amount member-shares) total-supply)
                u0))
        )
        (ok {
            shares: member-shares,
            estimated-dividend: estimated-dividend,
            last-claim: (get last-dividend-claim member-data)
        }))
        ERR_NOT_MEMBER
    )
)