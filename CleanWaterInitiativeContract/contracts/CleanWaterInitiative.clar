;; CleanWater Initiative Contract
;; Fund water projects globally with community verification and maintenance tracking

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-insufficient-funds (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-project-not-found (err u104))
(define-constant err-project-already-funded (err u105))

;; Data Variables
(define-data-var next-project-id uint u1)
(define-data-var total-funds-raised uint u0)

;; Data Maps
(define-map water-projects
    uint
    {
        project-name: (string-ascii 64),
        location: (string-ascii 64),
        funding-target: uint,
        funds-raised: uint,
        status: (string-ascii 20),
        creator: principal,
        verified: bool,
        maintenance-fund: uint,
    }
)

(define-map project-donations
    {
        project-id: uint,
        donor: principal,
    }
    uint
)
(define-map community-verifiers
    principal
    bool
)

;; Function 1: Fund Water Project
;; Allows users to contribute STX to fund water projects
(define-public (fund-water-project
        (project-id uint)
        (amount uint)
    )
    (let (
            (project (unwrap! (map-get? water-projects project-id) err-project-not-found))
            (current-funds-raised (get funds-raised project))
            (funding-target (get funding-target project))
            (current-donation (default-to u0
                (map-get? project-donations {
                    project-id: project-id,
                    donor: tx-sender,
                })
            ))
        )
        (begin
            ;; Validate input
            (asserts! (> amount u0) err-invalid-amount)
            (asserts! (< current-funds-raised funding-target)
                err-project-already-funded
            )

            ;; Transfer STX to contract
            (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

            ;; Update project funding
            (map-set water-projects project-id
                (merge project {
                    funds-raised: (+ current-funds-raised amount),
                    status: (if (>= (+ current-funds-raised amount) funding-target)
                        "funded"
                        "active"
                    ),
                })
            )

            ;; Track individual donations
            (map-set project-donations {
                project-id: project-id,
                donor: tx-sender,
            }
                (+ current-donation amount)
            )

            ;; Update total funds raised
            (var-set total-funds-raised (+ (var-get total-funds-raised) amount))

            ;; Print funding event
            (print {
                event: "project-funded",
                project-id: project-id,
                donor: tx-sender,
                amount: amount,
                total-raised: (+ current-funds-raised amount),
            })

            (ok true)
        )
    )
)

;; Function 2: Verify and Track Maintenance
;; Allows authorized community verifiers to verify project completion and track maintenance
(define-public (verify-and-track-maintenance
        (project-id uint)
        (verification-status bool)
        (maintenance-amount uint)
    )
    (let (
            (project (unwrap! (map-get? water-projects project-id) err-project-not-found))
            (is-verifier (default-to false (map-get? community-verifiers tx-sender)))
        )
        (begin
            ;; Check if sender is authorized verifier or contract owner
            (asserts! (or is-verifier (is-eq tx-sender contract-owner))
                err-not-authorized
            )
            (asserts! (>= maintenance-amount u0) err-invalid-amount)

            ;; Update project verification status and maintenance fund
            (map-set water-projects project-id
                (merge project {
                    verified: verification-status,
                    maintenance-fund: (+ (get maintenance-fund project) maintenance-amount),
                    status: (if verification-status
                        "verified"
                        (get status project)
                    ),
                })
            )

            ;; If maintenance amount is provided, transfer it to the project
            (if (> maintenance-amount u0)
                (try! (stx-transfer? maintenance-amount tx-sender
                    (as-contract tx-sender)
                ))
                true
            )

            ;; Print verification event
            (print {
                event: "project-verified",
                project-id: project-id,
                verifier: tx-sender,
                verified: verification-status,
                maintenance-added: maintenance-amount,
            })

            (ok {
                verified: verification-status,
                maintenance-fund: (+ (get maintenance-fund project) maintenance-amount),
            })
        )
    )
)

;; Read-only functions for project information
(define-read-only (get-project-details (project-id uint))
    (map-get? water-projects project-id)
)

(define-read-only (get-project-donation
        (project-id uint)
        (donor principal)
    )
    (map-get? project-donations {
        project-id: project-id,
        donor: donor,
    })
)

(define-read-only (get-total-funds-raised)
    (var-get total-funds-raised)
)

(define-read-only (is-community-verifier (verifier principal))
    (default-to false (map-get? community-verifiers verifier))
)

;; Administrative functions (owner only)
(define-public (create-water-project
        (project-name (string-ascii 64))
        (location (string-ascii 64))
        (funding-target uint)
    )
    (let ((project-id (var-get next-project-id)))
        (begin
            (asserts! (> funding-target u0) err-invalid-amount)

            (map-set water-projects project-id {
                project-name: project-name,
                location: location,
                funding-target: funding-target,
                funds-raised: u0,
                status: "active",
                creator: tx-sender,
                verified: false,
                maintenance-fund: u0,
            })

            (var-set next-project-id (+ project-id u1))
            (ok project-id)
        )
    )
)

(define-public (add-community-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set community-verifiers verifier true)
        (ok true)
    )
)