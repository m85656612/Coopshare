;; Member Performance Analytics
;; Tracks member engagement, participation patterns, and contribution history

(define-constant err-not-found (err u404))
(define-constant err-unauthorized (err u403))
(define-constant err-invalid-input (err u400))
(define-constant contract-owner tx-sender)

;; Member engagement metrics
(define-map member-engagement
  { member: principal }
  {
    participation-score: uint,
    votes-cast: uint,
    proposals-created: uint,
    features-requested: uint,
    shares-traded: uint,
    dividend-claims: uint,
    last-activity: uint,
    engagement-streak: uint
  }
)

;; Activity timeline tracking
(define-map activity-timeline
  { member: principal, period: uint }
  {
    activity-count: uint,
    vote-participation: uint,
    proposal-participation: uint,
    marketplace-participation: uint,
    share-activity: uint
  }
)

;; Member achievement tracking
(define-map member-achievements
  { member: principal }
  {
    governance-leader: bool,
    active-trader: bool,
    feature-contributor: bool,
    loyal-member: bool,
    community-builder: bool,
    achievement-count: uint
  }
)

;; Global analytics
(define-data-var current-period uint u1)
(define-data-var analytics-enabled bool true)
(define-data-var min-engagement-threshold uint u50)

;; Record member activity
(define-public (record-activity (member principal) (activity-type (string-ascii 20)) (activity-value uint))
  (let 
    (
      (current-metrics (default-to 
        { participation-score: u0, votes-cast: u0, proposals-created: u0,
          features-requested: u0, shares-traded: u0, dividend-claims: u0,
          last-activity: u0, engagement-streak: u0 }
        (map-get? member-engagement { member: member })))
      (period (var-get current-period))
      (period-activity (default-to 
        { activity-count: u0, vote-participation: u0, proposal-participation: u0,
          marketplace-participation: u0, share-activity: u0 }
        (map-get? activity-timeline { member: member, period: period })))
      (current-block stacks-block-height)
    )
    
    (asserts! (var-get analytics-enabled) err-unauthorized)
    
    ;; Update engagement metrics based on activity type
    (let 
      (
        (updated-metrics 
          (if (is-eq activity-type "vote")
            (merge current-metrics { 
              votes-cast: (+ (get votes-cast current-metrics) u1),
              participation-score: (+ (get participation-score current-metrics) u10)
            })
            (if (is-eq activity-type "proposal")
              (merge current-metrics { 
                proposals-created: (+ (get proposals-created current-metrics) u1),
                participation-score: (+ (get participation-score current-metrics) u25)
              })
              (if (is-eq activity-type "feature")
                (merge current-metrics { 
                  features-requested: (+ (get features-requested current-metrics) u1),
                  participation-score: (+ (get participation-score current-metrics) u15)
                })
                (if (is-eq activity-type "trade")
                  (merge current-metrics { 
                    shares-traded: (+ (get shares-traded current-metrics) activity-value),
                    participation-score: (+ (get participation-score current-metrics) u5)
                  })
                  (if (is-eq activity-type "dividend")
                    (merge current-metrics { 
                      dividend-claims: (+ (get dividend-claims current-metrics) u1),
                      participation-score: (+ (get participation-score current-metrics) u3)
                    })
                    current-metrics))))))
        (streak-update (if (< (- current-block (get last-activity updated-metrics)) u1440)
                        (+ (get engagement-streak updated-metrics) u1)
                        u1))
      )
      
      ;; Update member engagement
      (map-set member-engagement
        { member: member }
        (merge updated-metrics { 
          last-activity: current-block,
          engagement-streak: streak-update
        })
      )
      
      ;; Update period activity
      (map-set activity-timeline
        { member: member, period: period }
        (merge period-activity {
          activity-count: (+ (get activity-count period-activity) u1),
          vote-participation: (if (is-eq activity-type "vote") 
                               (+ (get vote-participation period-activity) u1)
                               (get vote-participation period-activity)),
          proposal-participation: (if (is-eq activity-type "proposal")
                                   (+ (get proposal-participation period-activity) u1)
                                   (get proposal-participation period-activity)),
          marketplace-participation: (if (is-eq activity-type "feature")
                                      (+ (get marketplace-participation period-activity) u1)
                                      (get marketplace-participation period-activity)),
          share-activity: (if (is-eq activity-type "trade")
                           (+ (get share-activity period-activity) activity-value)
                           (get share-activity period-activity))
        })
      )
      
      ;; Update achievements
      (try! (update-member-achievements member))
      
      (ok true)
    )
  )
)

;; Calculate member engagement level
(define-public (calculate-engagement-level (member principal))
  (let 
    (
      (metrics (unwrap! (map-get? member-engagement { member: member }) err-not-found))
      (score (get participation-score metrics))
      (level (if (>= score u500) "Expert"
               (if (>= score u200) "Advanced"
                 (if (>= score u100) "Intermediate" 
                   (if (>= score u50) "Active" "Beginner")))))
    )
    
    (asserts! (var-get analytics-enabled) err-unauthorized)
    (ok level)
  )
)

;; Update member achievements based on activity
(define-private (update-member-achievements (member principal))
  (let 
    (
      (metrics (unwrap! (map-get? member-engagement { member: member }) err-not-found))
      (current-achievements (default-to 
        { governance-leader: false, active-trader: false, feature-contributor: false,
          loyal-member: false, community-builder: false, achievement-count: u0 }
        (map-get? member-achievements { member: member })))
      (governance-leader (>= (+ (get votes-cast metrics) (get proposals-created metrics)) u20))
      (active-trader (>= (get shares-traded metrics) u100))
      (feature-contributor (>= (get features-requested metrics) u5))
      (loyal-member (>= (get engagement-streak metrics) u30))
      (community-builder (>= (get participation-score metrics) u300))
      (achievement-total (+ (if governance-leader u1 u0)
                           (+ (if active-trader u1 u0)
                              (+ (if feature-contributor u1 u0)
                                 (+ (if loyal-member u1 u0)
                                    (if community-builder u1 u0))))))
    )
    
    (map-set member-achievements
      { member: member }
      {
        governance-leader: governance-leader,
        active-trader: active-trader,
        feature-contributor: feature-contributor,
        loyal-member: loyal-member,
        community-builder: community-builder,
        achievement-count: achievement-total
      }
    )
    
    (ok achievement-total)
  )
)

;; Administrative functions
(define-public (update-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (var-set current-period new-period)
    (ok new-period)
  )
)

(define-public (toggle-analytics (enabled bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (var-set analytics-enabled enabled)
    (ok enabled)
  )
)

(define-public (set-engagement-threshold (threshold uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (> threshold u0) err-invalid-input)
    (var-set min-engagement-threshold threshold)
    (ok threshold)
  )
)

;; Read-only functions
(define-read-only (get-member-engagement (member principal))
  (map-get? member-engagement { member: member })
)

(define-read-only (get-member-achievements (member principal))
  (map-get? member-achievements { member: member })
)

(define-read-only (get-period-activity (member principal) (period uint))
  (map-get? activity-timeline { member: member, period: period })
)

(define-read-only (get-analytics-summary (member principal))
  (let 
    (
      (metrics (map-get? member-engagement { member: member }))
      (achievements (map-get? member-achievements { member: member }))
    )
    (match metrics
      data (ok {
        participation-score: (get participation-score data),
        total-activities: (+ (+ (get votes-cast data) (get proposals-created data))
                           (+ (get features-requested data) (get dividend-claims data))),
        engagement-streak: (get engagement-streak data),
        achievement-count: (match achievements 
                             achv (get achievement-count achv) 
                             u0),
        last-activity: (get last-activity data)
      })
      err-not-found
    )
  )
)

(define-read-only (get-analytics-config)
  {
    current-period: (var-get current-period),
    analytics-enabled: (var-get analytics-enabled),
    engagement-threshold: (var-get min-engagement-threshold)
  }
)
