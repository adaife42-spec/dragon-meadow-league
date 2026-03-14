;; Dragon Meadow League - Smart Contract
;; Clarity Version 2 | Epoch 2.1
;; Implements dual-token economy (DMT + DMG), skill ratings, and tournaments

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)

(define-constant ERR-NOT-AUTHORIZED        (err u100))
(define-constant ERR-ALREADY-REGISTERED   (err u101))
(define-constant ERR-NOT-REGISTERED       (err u102))
(define-constant ERR-TOURNAMENT-NOT-FOUND (err u103))
(define-constant ERR-TOURNAMENT-CLOSED    (err u104))
(define-constant ERR-INSUFFICIENT-BALANCE (err u105))
(define-constant ERR-INVALID-AMOUNT       (err u106))
(define-constant ERR-ALREADY-ENTERED      (err u107))
(define-constant ERR-TOURNAMENT-ACTIVE    (err u108))
(define-constant ERR-INVALID-PLACEMENT    (err u109))

;; Skill tier thresholds
(define-constant TIER-BRONZE-MAX   u999)
(define-constant TIER-SILVER-MAX   u1999)
(define-constant TIER-GOLD-MAX     u2999)
(define-constant TIER-PLATINUM-MAX u3999)
;; Diamond: 4000+

;; Revenue split basis points (out of 10000)
(define-constant PRIZE-POOL-BPS      u7000)   ;; 70% to prize pool
(define-constant ECOSYSTEM-BPS       u2000)   ;; 20% to ecosystem fund
(define-constant GOVERNANCE-BPS      u1000)   ;; 10% to governance rewards

;; DMG awarded per tournament based on placement
(define-constant DMG-FIRST-PLACE   u500)
(define-constant DMG-SECOND-PLACE  u300)
(define-constant DMG-THIRD-PLACE   u150)
(define-constant DMG-PARTICIPANT   u25)

;; Base skill rating for new players
(define-constant BASE-SKILL-RATING u1000)

;; ============================================================
;; DATA VARS
;; ============================================================

(define-data-var tournament-nonce uint u0)
(define-data-var ecosystem-fund   uint u0)
(define-data-var governance-fund  uint u0)

;; ============================================================
;; FUNGIBLE TOKENS
;; ============================================================

;; DMT - Dragon Meadow Token (utility / reward token)
(define-fungible-token dmt)

;; DMG - Dragon Meadow Governance Token
(define-fungible-token dmg)

;; ============================================================
;; DATA MAPS
;; ============================================================

;; Player profile
(define-map players
  principal
  {
    skill-rating:     uint,
    dmt-earned:       uint,
    dmg-earned:       uint,
    tournaments-played: uint,
    wins:             uint,
    registered-at:    uint    ;; block height
  }
)

;; Tournament record
(define-map tournaments
  uint   ;; tournament-id
  {
    name:         (string-ascii 64),
    mode:         (string-ascii 32),  ;; "strategy" | "puzzle" | "guild"
    entry-fee:    uint,
    prize-pool:   uint,
    min-tier:     uint,               ;; minimum skill rating to enter
    max-players:  uint,
    player-count: uint,
    status:       (string-ascii 16),  ;; "open" | "active" | "completed"
    created-at:   uint,
    created-by:   principal
  }
)

;; Tracks which players have entered a tournament
(define-map tournament-entries
  { tournament-id: uint, player: principal }
  bool
)

;; Tracks tournament placement results
(define-map tournament-results
  { tournament-id: uint, placement: uint }
  principal
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

(define-private (is-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (get-skill-tier (rating uint))
  (if (<= rating TIER-BRONZE-MAX)   "bronze"
  (if (<= rating TIER-SILVER-MAX)   "silver"
  (if (<= rating TIER-GOLD-MAX)     "gold"
  (if (<= rating TIER-PLATINUM-MAX) "platinum"
                                    "diamond"))))
)

(define-private (calc-bps (amount uint) (bps uint))
  (/ (* amount bps) u10000)
)

;; ============================================================
;; PLAYER REGISTRATION
;; ============================================================

;; Register a new player. Each wallet can only register once.
(define-public (register-player)
  (begin
    (asserts! (is-none (map-get? players tx-sender)) ERR-ALREADY-REGISTERED)
    (map-set players tx-sender
      {
        skill-rating:       BASE-SKILL-RATING,
        dmt-earned:         u0,
        dmg-earned:         u0,
        tournaments-played: u0,
        wins:               u0,
        registered-at:      block-height
      }
    )
    ;; Mint 100 DMT welcome bonus
    (try! (ft-mint? dmt u100 tx-sender))
    (ok true)
  )
)

;; ============================================================
;; TOKEN OPERATIONS
;; ============================================================

;; Transfer DMT between players
(define-public (transfer-dmt (amount uint) (recipient principal))
  (begin
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (is-some (map-get? players tx-sender))    ERR-NOT-REGISTERED)
    (asserts! (is-some (map-get? players recipient))    ERR-NOT-REGISTERED)
    (try! (ft-transfer? dmt amount tx-sender recipient))
    (ok true)
  )
)

;; Transfer DMG between players
(define-public (transfer-dmg (amount uint) (recipient principal))
  (begin
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (is-some (map-get? players tx-sender))    ERR-NOT-REGISTERED)
    (asserts! (is-some (map-get? players recipient))    ERR-NOT-REGISTERED)
    (try! (ft-transfer? dmg amount tx-sender recipient))
    (ok true)
  )
)

;; ============================================================
;; TOURNAMENT MANAGEMENT
;; ============================================================

;; Create a new tournament (owner only)
(define-public (create-tournament
    (name        (string-ascii 64))
    (mode        (string-ascii 32))
    (entry-fee   uint)
    (min-tier    uint)
    (max-players uint))
  (let ((tid (+ (var-get tournament-nonce) u1)))
    (asserts! (is-owner) ERR-NOT-AUTHORIZED)
    (asserts! (> max-players u1) ERR-INVALID-AMOUNT)
    (var-set tournament-nonce tid)
    (map-set tournaments tid
      {
        name:         name,
        mode:         mode,
        entry-fee:    entry-fee,
        prize-pool:   u0,
        min-tier:     min-tier,
        max-players:  max-players,
        player-count: u0,
        status:       "open",
        created-at:   block-height,
        created-by:   tx-sender
      }
    )
    (ok tid)
  )
)

;; Enter a tournament by paying the entry fee in DMT
(define-public (enter-tournament (tournament-id uint))
  (let (
    (player  (unwrap! (map-get? players tx-sender) ERR-NOT-REGISTERED))
    (tourney (unwrap! (map-get? tournaments tournament-id) ERR-TOURNAMENT-NOT-FOUND))
  )
    (asserts! (is-eq (get status tourney) "open")          ERR-TOURNAMENT-CLOSED)
    (asserts! (>= (get skill-rating player) (get min-tier tourney)) ERR-NOT-AUTHORIZED)
    (asserts! (< (get player-count tourney) (get max-players tourney)) ERR-TOURNAMENT-CLOSED)
    (asserts!
      (is-none (map-get? tournament-entries { tournament-id: tournament-id, player: tx-sender }))
      ERR-ALREADY-ENTERED
    )

    ;; Collect entry fee: split to prize pool, ecosystem, governance
    (let (
      (fee           (get entry-fee tourney))
      (to-prize      (calc-bps fee PRIZE-POOL-BPS))
      (to-ecosystem  (calc-bps fee ECOSYSTEM-BPS))
      (to-governance (calc-bps fee GOVERNANCE-BPS))
    )
      (if (> fee u0)
        (try! (ft-transfer? dmt fee tx-sender CONTRACT-OWNER))
        true
      )
      (map-set tournaments tournament-id
        (merge tourney {
          prize-pool:   (+ (get prize-pool tourney) to-prize),
          player-count: (+ (get player-count tourney) u1)
        })
      )
      (var-set ecosystem-fund (+ (var-get ecosystem-fund) to-ecosystem))
      (var-set governance-fund (+ (var-get governance-fund) to-governance))
      (map-set tournament-entries
        { tournament-id: tournament-id, player: tx-sender }
        true
      )
      (ok true)
    )
  )
)

;; ============================================================
;; TOURNAMENT RESULTS & REWARD DISTRIBUTION
;; ============================================================

;; Finalize a tournament and distribute rewards (owner only).
;; Provide the top 3 finishers; all other entrants receive participant DMG.
(define-public (finalize-tournament
    (tournament-id uint)
    (first-place   principal)
    (second-place  principal)
    (third-place   principal))
  (let (
    (tourney (unwrap! (map-get? tournaments tournament-id) ERR-TOURNAMENT-NOT-FOUND))
  )
    (asserts! (is-owner) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-eq (get status tourney) "completed")) ERR-TOURNAMENT-CLOSED)

    ;; Verify entrants
    (asserts!
      (is-some (map-get? tournament-entries { tournament-id: tournament-id, player: first-place }))
      ERR-INVALID-PLACEMENT
    )
    (asserts!
      (is-some (map-get? tournament-entries { tournament-id: tournament-id, player: second-place }))
      ERR-INVALID-PLACEMENT
    )
    (asserts!
      (is-some (map-get? tournament-entries { tournament-id: tournament-id, player: third-place }))
      ERR-INVALID-PLACEMENT
    )

    (let (
      (prize-pool   (get prize-pool tourney))
      (dmt-first    (calc-bps prize-pool u5000))  ;; 50% of prize pool
      (dmt-second   (calc-bps prize-pool u3000))  ;; 30%
      (dmt-third    (calc-bps prize-pool u2000))  ;; 20%
    )
      ;; Distribute DMT prizes
      (if (> dmt-first u0)  (try! (ft-transfer? dmt dmt-first  CONTRACT-OWNER first-place))  true)
      (if (> dmt-second u0) (try! (ft-transfer? dmt dmt-second CONTRACT-OWNER second-place)) true)
      (if (> dmt-third u0)  (try! (ft-transfer? dmt dmt-third  CONTRACT-OWNER third-place))  true)

      ;; Distribute DMG governance tokens
      (try! (ft-mint? dmg DMG-FIRST-PLACE  first-place))
      (try! (ft-mint? dmg DMG-SECOND-PLACE second-place))
      (try! (ft-mint? dmg DMG-THIRD-PLACE  third-place))

      ;; Record placements
      (map-set tournament-results { tournament-id: tournament-id, placement: u1 } first-place)
      (map-set tournament-results { tournament-id: tournament-id, placement: u2 } second-place)
      (map-set tournament-results { tournament-id: tournament-id, placement: u3 } third-place)

      ;; Update winner profiles
      (update-player-after-tournament first-place  u200 true)
      (update-player-after-tournament second-place u100 false)
      (update-player-after-tournament third-place  u50  false)

      ;; Mark tournament completed
      (map-set tournaments tournament-id (merge tourney { status: "completed" }))
      (ok true)
    )
  )
)

;; Internal: update player stats and skill rating after a tournament
(define-private (update-player-after-tournament
    (player   principal)
    (rating-delta uint)
    (is-winner    bool))
  (match (map-get? players player)
    profile
      (map-set players player
        (merge profile {
          skill-rating:       (+ (get skill-rating profile) rating-delta),
          tournaments-played: (+ (get tournaments-played profile) u1),
          wins:               (if is-winner
                                (+ (get wins profile) u1)
                                (get wins profile))
        })
      )
    false  ;; player not found, no-op
  )
)

;; ============================================================
;; GOVERNANCE: SKILL-WEIGHTED VOTING POWER
;; ============================================================

;; Returns a player's voting power = skill-rating * dmg-balance
;; This reflects the Play-to-Govern model.
(define-read-only (get-voting-power (player principal))
  (match (map-get? players player)
    profile
      (let ((dmg-bal (ft-get-balance dmg player)))
        (ok (* (get skill-rating profile) dmg-bal))
      )
    ERR-NOT-REGISTERED
  )
)

;; ============================================================
;; ADMIN: MINT DMT REWARDS (owner only)
;; ============================================================

;; Mint DMT to a player as a skill-demonstration reward
(define-public (mint-skill-reward (recipient principal) (amount uint))
  (begin
    (asserts! (is-owner) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (is-some (map-get? players recipient)) ERR-NOT-REGISTERED)
    (try! (ft-mint? dmt amount recipient))
    ;; Record in player profile
    (match (map-get? players recipient)
      profile
        (map-set players recipient
          (merge profile { dmt-earned: (+ (get dmt-earned profile) amount) })
        )
      false
    )
    (ok true)
  )
)

;; ============================================================
;; READ-ONLY VIEWS
;; ============================================================

(define-read-only (get-player (player principal))
  (map-get? players player)
)

(define-read-only (get-player-tier (player principal))
  (match (map-get? players player)
    profile (ok (get-skill-tier (get skill-rating profile)))
    ERR-NOT-REGISTERED
  )
)

(define-read-only (get-tournament (tournament-id uint))
  (map-get? tournaments tournament-id)
)

(define-read-only (get-tournament-winner (tournament-id uint))
  (map-get? tournament-results { tournament-id: tournament-id, placement: u1 })
)

(define-read-only (get-dmt-balance (player principal))
  (ok (ft-get-balance dmt player))
)

(define-read-only (get-dmg-balance (player principal))
  (ok (ft-get-balance dmg player))
)

(define-read-only (get-ecosystem-fund)
  (ok (var-get ecosystem-fund))
)

(define-read-only (get-governance-fund)
  (ok (var-get governance-fund))
)

(define-read-only (get-total-tournaments)
  (ok (var-get tournament-nonce))
)

(define-read-only (is-tournament-entrant (tournament-id uint) (player principal))
  (ok (default-to false
    (map-get? tournament-entries { tournament-id: tournament-id, player: player })))
)
