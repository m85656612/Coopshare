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
(define-constant ERR_INSUFFICIENT_STAKE (err u111))
(define-constant ERR_STAKE_LOCKED (err u112))
(define-constant ERR_INVALID_DURATION (err u113))
(define-constant ERR_STAKE_NOT_FOUND (err u114))
(define-constant ERR_ALREADY_STAKED (err u115))
(define-constant ERR_EARLY_WITHDRAWAL (err u116))
(define-constant ERR_REWARD_POOL_EMPTY (err u117))
(define-constant ERR_FEATURE_NOT_FOUND (err u118))
(define-constant ERR_ALREADY_BIDDING (err u119))
(define-constant ERR_BID_NOT_FOUND (err u120))
(define-constant ERR_FEATURE_COMPLETED (err u121))
(define-constant ERR_NOT_FEATURE_OWNER (err u122))
(define-constant ERR_NOT_BID_OWNER (err u123))
(define-constant ERR_INSUFFICIENT_BOUNTY (err u124))
(define-constant ERR_FEATURE_ACTIVE (err u125))
(define-constant ERR_NO_BIDS (err u126))

(define-data-var total-shares uint u0)
(define-data-var total-members uint u0)
(define-data-var proposal-counter uint u0)
(define-data-var dividend-pool uint u0)
(define-data-var last-dividend-block uint u0)
(define-data-var total-staked-shares uint u0)
(define-data-var reward-pool uint u0)
(define-data-var stake-counter uint u0)
(define-data-var reward-rate uint u100)
(define-data-var min-stake-duration uint u144)
(define-data-var max-stake-duration uint u52560)
(define-data-var early-withdrawal-penalty uint u10)
(define-data-var feature-counter uint u0)
(define-data-var bid-counter uint u0)

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

(define-map stakes uint {
    staker: principal,
    amount: uint,
    duration: uint,
    created-at: uint,
    unlock-block: uint,
    reward-rate: uint,
    active: bool
})

(define-map user-stakes principal (list 50 uint))

(define-map stake-rewards principal uint)

(define-map feature-requests uint {
    title: (string-ascii 100),
    description: (string-ascii 500),
    bounty: uint,
    requester: principal,
    assignee: (optional principal),
    created-at: uint,
    deadline: uint,
    status: (string-ascii 20),
    category: (string-ascii 50),
    priority: uint
})

(define-map feature-bids uint {
    feature-id: uint,
    bidder: principal,
    amount: uint,
    timeline: uint,
    proposal: (string-ascii 300),
    created-at: uint,
    status: (string-ascii 20)
})

(define-map feature-bidders {feature-id: uint, bidder: principal} uint)
(define-map user-feature-bids principal (list 100 uint))
(define-map feature-bid-list uint (list 50 uint))

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

(define-public (stake-shares (amount uint) (duration uint))
    (let (
        (caller tx-sender)
        (member-data (unwrap! (map-get? members caller) ERR_NOT_MEMBER))
        (current-block stacks-block-height)
        (stake-id (+ (var-get stake-counter) u1))
        (unlock-block (+ current-block duration))
        (member-shares (get shares member-data))
        (current-stakes (default-to (list) (map-get? user-stakes caller)))
        (bonus-rate (if (>= duration u1440) u150 u100))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= duration (var-get min-stake-duration)) ERR_INVALID_DURATION)
    (asserts! (<= duration (var-get max-stake-duration)) ERR_INVALID_DURATION)
    (asserts! (>= member-shares amount) ERR_INSUFFICIENT_SHARES)
    (asserts! (< (len current-stakes) u50) ERR_ALREADY_STAKED)
    
    (map-set stakes stake-id {
        staker: caller,
        amount: amount,
        duration: duration,
        created-at: current-block,
        unlock-block: unlock-block,
        reward-rate: bonus-rate,
        active: true
    })
    
    (map-set members caller (merge member-data {
        shares: (- member-shares amount)
    }))
    
    (map-set user-stakes caller (unwrap! (as-max-len? (append current-stakes stake-id) u50) ERR_ALREADY_STAKED))
    
    (var-set stake-counter stake-id)
    (var-set total-staked-shares (+ (var-get total-staked-shares) amount))
    
    (ok stake-id)
    )
)

(define-public (unstake-shares (stake-id uint))
    (let (
        (caller tx-sender)
        (stake-data (unwrap! (map-get? stakes stake-id) ERR_STAKE_NOT_FOUND))
        (member-data (unwrap! (map-get? members caller) ERR_NOT_MEMBER))
        (current-block stacks-block-height)
        (stake-amount (get amount stake-data))
        (unlock-block (get unlock-block stake-data))
        (staker (get staker stake-data))
        (is-active (get active stake-data))
        (early-withdrawal (< current-block unlock-block))
        (penalty-amount (if early-withdrawal (/ (* stake-amount (var-get early-withdrawal-penalty)) u100) u0))
        (final-amount (- stake-amount penalty-amount))
        (current-stakes (default-to (list) (map-get? user-stakes caller)))
        (updated-stakes (filter is-not-stake-id current-stakes))
    )
    (asserts! (is-eq caller staker) ERR_UNAUTHORIZED)
    (asserts! is-active ERR_STAKE_NOT_FOUND)
    
    (map-set stakes stake-id (merge stake-data {
        active: false
    }))
    
    (map-set members caller (merge member-data {
        shares: (+ (get shares member-data) final-amount)
    }))
    
    (map-set user-stakes caller updated-stakes)
    
    (var-set total-staked-shares (- (var-get total-staked-shares) stake-amount))
    
    (if early-withdrawal
        (var-set reward-pool (+ (var-get reward-pool) penalty-amount))
        true
    )
    
    (ok final-amount)
    )
)

(define-public (claim-stake-rewards)
    (let (
        (caller tx-sender)
        (member-data (unwrap! (map-get? members caller) ERR_NOT_MEMBER))
        (current-rewards (default-to u0 (map-get? stake-rewards caller)))
        (total-rewards (calculate-total-rewards-for-user caller))
        (claimable-amount (- total-rewards current-rewards))
        (pool-balance (var-get reward-pool))
    )
    (asserts! (> claimable-amount u0) ERR_NO_DIVIDENDS)
    (asserts! (>= pool-balance claimable-amount) ERR_REWARD_POOL_EMPTY)
    
    (try! (as-contract (stx-transfer? claimable-amount tx-sender caller)))
    
    (map-set stake-rewards caller total-rewards)
    (var-set reward-pool (- pool-balance claimable-amount))
    
    (ok claimable-amount)
    )
)

(define-public (add-to-reward-pool (amount uint))
    (let (
        (caller tx-sender)
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (stx-get-balance caller) amount) ERR_INSUFFICIENT_FUNDS)
    
    (try! (stx-transfer? amount caller (as-contract tx-sender)))
    (var-set reward-pool (+ (var-get reward-pool) amount))
    
    (ok amount)
    )
)

(define-public (update-reward-rate (new-rate uint))
    (let (
        (caller tx-sender)
    )
    (asserts! (is-eq caller CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> new-rate u0) ERR_INVALID_AMOUNT)
    (asserts! (<= new-rate u1000) ERR_INVALID_AMOUNT)
    
    (var-set reward-rate new-rate)
    
    (ok new-rate)
    )
)

(define-public (update-stake-parameters (min-duration uint) (max-duration uint) (penalty-rate uint))
    (let (
        (caller tx-sender)
    )
    (asserts! (is-eq caller CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> min-duration u0) ERR_INVALID_DURATION)
    (asserts! (< min-duration max-duration) ERR_INVALID_DURATION)
    (asserts! (<= penalty-rate u100) ERR_INVALID_AMOUNT)
    
    (var-set min-stake-duration min-duration)
    (var-set max-stake-duration max-duration)
    (var-set early-withdrawal-penalty penalty-rate)
    
    (ok true)
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

;; Feature Marketplace Functions

(define-public (create-feature-request (title (string-ascii 100)) (description (string-ascii 500)) (bounty uint) (category (string-ascii 50)) (priority uint) (deadline-blocks uint))
    (let (
        (caller tx-sender)
        (member-data (unwrap! (map-get? members caller) ERR_NOT_MEMBER))
        (feature-id (+ (var-get feature-counter) u1))
        (current-block stacks-block-height)
        (deadline-block (+ current-block deadline-blocks))
    )
    (asserts! (> bounty u0) ERR_INVALID_AMOUNT)
    (asserts! (> priority u0) ERR_INVALID_AMOUNT)
    (asserts! (<= priority u5) ERR_INVALID_AMOUNT)
    (asserts! (>= (stx-get-balance caller) bounty) ERR_INSUFFICIENT_FUNDS)
    
    (try! (stx-transfer? bounty caller (as-contract tx-sender)))
    
    (map-set feature-requests feature-id {
        title: title,
        description: description,
        bounty: bounty,
        requester: caller,
        assignee: none,
        created-at: current-block,
        deadline: deadline-block,
        status: "open",
        category: category,
        priority: priority
    })
    
    (map-set feature-bid-list feature-id (list))
    (var-set feature-counter feature-id)
    
    (ok feature-id)
    )
)

(define-public (submit-bid (feature-id uint) (bid-amount uint) (timeline-blocks uint) (proposal (string-ascii 300)))
    (let (
        (caller tx-sender)
        (member-data (unwrap! (map-get? members caller) ERR_NOT_MEMBER))
        (feature-data (unwrap! (map-get? feature-requests feature-id) ERR_FEATURE_NOT_FOUND))
        (bid-id (+ (var-get bid-counter) u1))
        (current-block stacks-block-height)
        (bidder-key {feature-id: feature-id, bidder: caller})
        (current-bids (default-to (list) (map-get? user-feature-bids caller)))
        (existing-feature-bids (default-to (list) (map-get? feature-bid-list feature-id)))
    )
    (asserts! (is-eq (get status feature-data) "open") ERR_FEATURE_COMPLETED)
    (asserts! (> bid-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> timeline-blocks u0) ERR_INVALID_DURATION)
    (asserts! (< current-block (get deadline feature-data)) ERR_VOTING_ENDED)
    (asserts! (is-none (map-get? feature-bidders bidder-key)) ERR_ALREADY_BIDDING)
    (asserts! (< (len current-bids) u100) ERR_ALREADY_BIDDING)
    (asserts! (< (len existing-feature-bids) u50) ERR_ALREADY_BIDDING)
    
    (map-set feature-bids bid-id {
        feature-id: feature-id,
        bidder: caller,
        amount: bid-amount,
        timeline: timeline-blocks,
        proposal: proposal,
        created-at: current-block,
        status: "pending"
    })
    
    (map-set feature-bidders bidder-key bid-id)
    (map-set user-feature-bids caller (unwrap! (as-max-len? (append current-bids bid-id) u100) ERR_ALREADY_BIDDING))
    (map-set feature-bid-list feature-id (unwrap! (as-max-len? (append existing-feature-bids bid-id) u50) ERR_ALREADY_BIDDING))
    
    (var-set bid-counter bid-id)
    
    (ok bid-id)
    )
)

(define-public (accept-bid (feature-id uint) (bid-id uint))
    (let (
        (caller tx-sender)
        (feature-data (unwrap! (map-get? feature-requests feature-id) ERR_FEATURE_NOT_FOUND))
        (bid-data (unwrap! (map-get? feature-bids bid-id) ERR_BID_NOT_FOUND))
        (requester (get requester feature-data))
        (bidder (get bidder bid-data))
        (current-block stacks-block-height)
    )
    (asserts! (is-eq caller requester) ERR_NOT_FEATURE_OWNER)
    (asserts! (is-eq (get feature-id bid-data) feature-id) ERR_BID_NOT_FOUND)
    (asserts! (is-eq (get status feature-data) "open") ERR_FEATURE_COMPLETED)
    (asserts! (is-eq (get status bid-data) "pending") ERR_FEATURE_COMPLETED)
    
    (map-set feature-requests feature-id (merge feature-data {
        assignee: (some bidder),
        status: "assigned"
    }))
    
    (map-set feature-bids bid-id (merge bid-data {
        status: "accepted"
    }))
    
    (ok true)
    )
)

(define-public (complete-feature (feature-id uint))
    (let (
        (caller tx-sender)
        (feature-data (unwrap! (map-get? feature-requests feature-id) ERR_FEATURE_NOT_FOUND))
        (assignee (unwrap! (get assignee feature-data) ERR_NOT_BID_OWNER))
        (bounty-amount (get bounty feature-data))
        (current-block stacks-block-height)
    )
    (asserts! (is-eq caller assignee) ERR_NOT_BID_OWNER)
    (asserts! (is-eq (get status feature-data) "assigned") ERR_FEATURE_COMPLETED)
    
    (try! (as-contract (stx-transfer? bounty-amount tx-sender caller)))
    
    (map-set feature-requests feature-id (merge feature-data {
        status: "completed"
    }))
    
    (ok bounty-amount)
    )
)

(define-public (cancel-feature-request (feature-id uint))
    (let (
        (caller tx-sender)
        (feature-data (unwrap! (map-get? feature-requests feature-id) ERR_FEATURE_NOT_FOUND))
        (requester (get requester feature-data))
        (bounty-amount (get bounty feature-data))
        (current-status (get status feature-data))
    )
    (asserts! (is-eq caller requester) ERR_NOT_FEATURE_OWNER)
    (asserts! (is-eq current-status "open") ERR_FEATURE_ACTIVE)
    
    (try! (as-contract (stx-transfer? bounty-amount tx-sender caller)))
    
    (map-set feature-requests feature-id (merge feature-data {
        status: "cancelled"
    }))
    
    (ok bounty-amount)
    )
)

(define-public (withdraw-bid (bid-id uint))
    (let (
        (caller tx-sender)
        (bid-data (unwrap! (map-get? feature-bids bid-id) ERR_BID_NOT_FOUND))
        (feature-id (get feature-id bid-data))
        (feature-data (unwrap! (map-get? feature-requests feature-id) ERR_FEATURE_NOT_FOUND))
        (bidder (get bidder bid-data))
        (bidder-key {feature-id: feature-id, bidder: caller})
        (current-bids (default-to (list) (map-get? user-feature-bids caller)))
        (updated-bids (filter is-not-bid-id current-bids))
    )
    (asserts! (is-eq caller bidder) ERR_NOT_BID_OWNER)
    (asserts! (is-eq (get status bid-data) "pending") ERR_FEATURE_COMPLETED)
    (asserts! (is-eq (get status feature-data) "open") ERR_FEATURE_COMPLETED)
    
    (map-set feature-bids bid-id (merge bid-data {
        status: "withdrawn"
    }))
    
    (map-delete feature-bidders bidder-key)
    (map-set user-feature-bids caller updated-bids)
    
    (ok true)
    )
)

(define-public (rate-completed-feature (feature-id uint) (rating uint))
    (let (
        (caller tx-sender)
        (feature-data (unwrap! (map-get? feature-requests feature-id) ERR_FEATURE_NOT_FOUND))
        (requester (get requester feature-data))
    )
    (asserts! (is-eq caller requester) ERR_NOT_FEATURE_OWNER)
    (asserts! (is-eq (get status feature-data) "completed") ERR_FEATURE_ACTIVE)
    (asserts! (>= rating u1) ERR_INVALID_AMOUNT)
    (asserts! (<= rating u5) ERR_INVALID_AMOUNT)
    
    (ok rating)
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

(define-private (is-not-stake-id (stake-id uint))
    (not (is-eq stake-id stake-id))
)

(define-private (is-not-bid-id (bid-id uint))
    (not (is-eq bid-id bid-id))
)

(define-private (calculate-total-rewards-for-user (user principal))
    (let (
        (user-stakes-list (default-to (list) (map-get? user-stakes user)))
        (total-rewards (fold calculate-stake-reward user-stakes-list u0))
    )
    total-rewards
    )
)

(define-private (calculate-stake-reward (stake-id uint) (accumulated-rewards uint))
    (match (map-get? stakes stake-id)
        stake-data (let (
            (stake-amount (get amount stake-data))
            (stake-reward-rate (get reward-rate stake-data))
            (created-at (get created-at stake-data))
            (current-block stacks-block-height)
            (blocks-elapsed (- current-block created-at))
            (stake-reward (/ (* stake-amount stake-reward-rate blocks-elapsed) u100000))
        )
        (+ accumulated-rewards stake-reward))
        accumulated-rewards
    )
)

(define-read-only (get-stake-info (stake-id uint))
    (map-get? stakes stake-id)
)

(define-read-only (get-user-stakes (user principal))
    (map-get? user-stakes user)
)

(define-read-only (get-total-staked-shares)
    (ok (var-get total-staked-shares))
)

(define-read-only (get-reward-pool)
    (ok (var-get reward-pool))
)

(define-read-only (get-stake-parameters)
    (ok {
        min-duration: (var-get min-stake-duration),
        max-duration: (var-get max-stake-duration),
        penalty-rate: (var-get early-withdrawal-penalty),
        reward-rate: (var-get reward-rate)
    })
)

(define-read-only (get-user-claimable-rewards (user principal))
    (let (
        (current-rewards (default-to u0 (map-get? stake-rewards user)))
        (total-rewards (calculate-total-rewards-for-user user))
        (claimable-amount (- total-rewards current-rewards))
    )
    (ok claimable-amount)
    )
)

(define-read-only (calculate-staking-voting-power (member principal))
    (let (
        (member-data (unwrap! (map-get? members member) ERR_NOT_MEMBER))
        (regular-shares (get shares member-data))
        (user-stakes-list (default-to (list) (map-get? user-stakes member)))
        (staked-voting-power (fold calculate-stake-voting-power user-stakes-list u0))
    )
    (ok (+ regular-shares staked-voting-power))
    )
)

(define-private (calculate-stake-voting-power (stake-id uint) (accumulated-power uint))
    (match (map-get? stakes stake-id)
        stake-data (let (
            (stake-amount (get amount stake-data))
            (stake-reward-rate (get reward-rate stake-data))
            (multiplier (if (>= stake-reward-rate u150) u2 u1))
            (voting-power (* stake-amount multiplier))
        )
        (+ accumulated-power voting-power))
        accumulated-power
    )
)

(define-read-only (get-stake-count)
    (ok (var-get stake-counter))
)

(define-read-only (preview-stake-rewards (amount uint) (duration uint))
    (let (
        (bonus-rate (if (>= duration u1440) u150 u100))
        (estimated-reward (/ (* amount bonus-rate duration) u100000))
    )
    (ok {
        reward-rate: bonus-rate,
        estimated-reward: estimated-reward,
        voting-multiplier: (if (>= bonus-rate u150) u2 u1)
    })
    )
)

;; Feature Marketplace Read-Only Functions

(define-read-only (get-feature-request (feature-id uint))
    (map-get? feature-requests feature-id)
)

(define-read-only (get-feature-bid (bid-id uint))
    (map-get? feature-bids bid-id)
)

(define-read-only (get-feature-bids (feature-id uint))
    (map-get? feature-bid-list feature-id)
)

(define-read-only (get-user-feature-bids (user principal))
    (map-get? user-feature-bids user)
)

(define-read-only (get-feature-count)
    (ok (var-get feature-counter))
)

(define-read-only (get-bid-count)
    (ok (var-get bid-counter))
)

(define-read-only (get-feature-status (feature-id uint))
    (match (map-get? feature-requests feature-id)
        feature-data (let (
            (current-block stacks-block-height)
            (deadline-passed (>= current-block (get deadline feature-data)))
            (status (get status feature-data))
            (bid-count (len (default-to (list) (map-get? feature-bid-list feature-id))))
        )
        (ok {
            status: status,
            deadline-passed: deadline-passed,
            bid-count: bid-count,
            bounty: (get bounty feature-data),
            priority: (get priority feature-data)
        }))
        ERR_FEATURE_NOT_FOUND
    )
)

(define-read-only (get-marketplace-stats)
    (let (
        (total-features (var-get feature-counter))
        (total-bids (var-get bid-counter))
    )
    (ok {
        total-features: total-features,
        total-bids: total-bids,
        active-features: u0,
        completed-features: u0
    })
    )
)

(define-read-only (calculate-bid-score (bid-id uint))
    (match (map-get? feature-bids bid-id)
        bid-data (let (
            (bid-amount (get amount bid-data))
            (timeline (get timeline bid-data))
            (feature-id (get feature-id bid-data))
            (feature-data (unwrap! (map-get? feature-requests feature-id) ERR_FEATURE_NOT_FOUND))
            (bounty (get bounty feature-data))
            (savings (- bounty bid-amount))
            (efficiency-score (if (> timeline u0) (/ savings timeline) u0))
        )
        (ok {
            savings: savings,
            timeline: timeline,
            efficiency-score: efficiency-score,
            bid-amount: bid-amount
        }))
        ERR_BID_NOT_FOUND
    )
)



