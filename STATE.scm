;;; STATE.scm - Well-Known Ecosystem Project State
;;; Format: Guile Scheme S-expressions
;;; Reference: https://github.com/hyperpolymath/state.scm

;;;============================================================================
;;; METADATA
;;;============================================================================

(define-module (state well-known-ecosystem)
  #:export (state))

(define metadata
  '((version . "1.0.0")
    (created . "2025-12-08")
    (last-modified . "2025-12-08")
    (project-name . "well-known-ecosystem")
    (repository . "https://github.com/hyperpolymath/well-known-ecosystem")))

;;;============================================================================
;;; CURRENT POSITION
;;;============================================================================

(define current-position
  '((phase . "initialization")
    (completion-percentage . 5)
    (status . "scaffolding-complete")

    (what-exists
      ((github-infrastructure
         (workflows
           (jekyll-gh-pages . "configured")  ; GitHub Pages deployment
           (codeql . "configured"))          ; Security scanning
         (dependabot . "configured-incomplete") ; Package ecosystem not specified
         (issue-templates
           (bug-report . "present")
           (feature-request . "present")
           (custom . "empty")))
       (repository-basics
         (gitignore . "present-but-empty")
         (readme . "missing")
         (license . "missing")
         (documentation . "missing"))))

    (what-is-missing
      ((core-content
         (readme . "critical")
         (license . "critical")
         (index-page . "critical")
         (well-known-registry . "critical"))
       (project-definition
         (purpose-statement . "undefined")
         (scope . "undefined")
         (governance . "undefined"))))))

;;;============================================================================
;;; ROUTE TO MVP v1
;;;============================================================================

(define mvp-v1-route
  '((goal . "Establish a comprehensive registry and documentation hub for .well-known URIs")

    (phase-1 "Foundation"
      ((priority . 1)
       (tasks
         (define-project-scope
           (description . "Clarify what 'well-known ecosystem' means")
           (deliverable . "README.md with clear mission statement")
           (status . "pending"))
         (add-license
           (description . "Choose appropriate open-source license")
           (options . ("MIT" "Apache-2.0" "CC-BY-4.0"))
           (status . "pending"))
         (create-index
           (description . "Jekyll index.html or index.md")
           (deliverable . "Landing page for GitHub Pages")
           (status . "pending")))))

    (phase-2 "Content Structure"
      ((priority . 2)
       (tasks
         (design-registry-format
           (description . "Define schema for .well-known URI entries")
           (fields . ("uri-suffix" "rfc-reference" "description" "mime-type" "examples"))
           (format-options . ("JSON" "YAML" "TOML"))
           (status . "pending"))
         (create-directory-structure
           (description . "Organize content directories")
           (proposed-structure
             ("/_data/well-known-uris.yml" . "Registry data")
             ("/_includes/" . "Reusable components")
             ("/_layouts/" . "Page templates")
             ("/docs/" . "Extended documentation")
             ("/examples/" . "Implementation examples"))
           (status . "pending"))
         (seed-initial-entries
           (description . "Populate with common .well-known URIs")
           (examples . ("security.txt" "robots.txt" "openid-configuration"
                        "webfinger" "nodeinfo" "apple-app-site-association"))
           (status . "pending")))))

    (phase-3 "MVP Launch"
      ((priority . 3)
       (tasks
         (fix-dependabot-config
           (description . "Specify package ecosystem (likely 'bundler' for Jekyll)")
           (file . ".github/dependabot.yml")
           (status . "pending"))
         (create-contributing-guide
           (description . "How to submit new .well-known URIs")
           (status . "pending"))
         (deploy-and-test
           (description . "Verify GitHub Pages deployment works")
           (status . "pending"))
         (announce-mvp
           (description . "Share with relevant communities")
           (status . "pending")))))))

;;;============================================================================
;;; ISSUES / BLOCKERS
;;;============================================================================

(define current-issues
  '((critical
      ((issue-1
         (title . "Project purpose undefined")
         (description . "Repository name suggests .well-known ecosystem but
                         no documentation explains the actual goal")
         (impact . "Cannot make architectural decisions without clarity")
         (resolution . "Owner must define scope and purpose"))
       (issue-2
         (title . "No content exists")
         (description . "Repository is pure boilerplate with zero project-specific content")
         (impact . "GitHub Pages will deploy an empty/broken site")
         (resolution . "Create index.html/md and basic content structure"))))

    (moderate
      ((issue-3
         (title . "Dependabot misconfigured")
         (description . "package-ecosystem is empty string in dependabot.yml")
         (file . ".github/dependabot.yml:8")
         (resolution . "Set to 'bundler' for Jekyll or 'github-actions' for workflows"))
       (issue-4
         (title . "Empty gitignore")
         (description . ".gitignore exists but has no rules")
         (resolution . "Add Jekyll-specific ignores: _site/, .jekyll-cache/, etc."))
       (issue-5
         (title . "Custom issue template is empty")
         (description . "custom.md template has no content")
         (file . ".github/ISSUE_TEMPLATE/custom.md")
         (resolution . "Either populate or remove"))))

    (low
      ((issue-6
         (title . "No CODEOWNERS file")
         (description . "No automatic review assignment")
         (resolution . "Add .github/CODEOWNERS if team grows"))))))

;;;============================================================================
;;; QUESTIONS FOR PROJECT OWNER
;;;============================================================================

(define questions-for-owner
  '((scope-questions
      ((q1 . "Is this meant to be a registry/catalog of ALL .well-known URIs
              (like IANA's list), or focused on specific ones?")
       (q2 . "Should this include implementation guides, or just reference documentation?")
       (q3 . "Is the target audience developers, sysadmins, or standards bodies?")))

    (technical-questions
      ((q4 . "Preferred data format for the registry: JSON, YAML, or structured Markdown?")
       (q5 . "Should entries link to official RFCs/specs, or include inline documentation?")
       (q6 . "Any preference on Jekyll theme or custom design?")))

    (governance-questions
      ((q7 . "What license should this project use? (MIT, Apache-2.0, CC-BY-4.0 for docs)")
       (q8 . "Will this accept community contributions for new .well-known URIs?")
       (q9 . "Should there be a formal review process for additions?")))

    (integration-questions
      ((q10 . "Should this integrate with or reference the IANA well-known URIs registry?")
       (q11 . "Any plans to provide machine-readable exports (JSON API, etc.)?")
       (q12 . "Should there be validation tooling for .well-known implementations?")))))

;;;============================================================================
;;; LONG-TERM ROADMAP
;;;============================================================================

(define long-term-roadmap
  '((v1.0 "MVP - Static Registry"
      ((timeline . "foundation")
       (goals
         ("Comprehensive .well-known URI catalog")
         ("Clean documentation site via GitHub Pages")
         ("Community contribution workflow"))
       (deliverables
         ("Searchable registry of 20+ .well-known URIs")
         ("RFC/spec references for each entry")
         ("Basic implementation examples"))))

    (v1.5 "Enhanced Documentation"
      ((timeline . "post-mvp")
       (goals
         ("Deep-dive guides for popular .well-known URIs")
         ("Platform-specific implementation tutorials")
         ("Common pitfalls and troubleshooting"))
       (deliverables
         ("Tutorial: Implementing security.txt")
         ("Tutorial: OpenID Connect Discovery")
         ("Tutorial: WebFinger for federation")
         ("Server configuration examples (nginx, Apache, Caddy)"))))

    (v2.0 "Tooling & Validation"
      ((timeline . "expansion")
       (goals
         ("Provide validation tools for implementations")
         ("API for programmatic access")
         ("Community testing infrastructure"))
       (deliverables
         ("Online validator for .well-known files")
         ("JSON API endpoint for registry data")
         ("CLI tool for local validation")
         ("GitHub Action for CI validation"))))

    (v3.0 "Ecosystem Integration"
      ((timeline . "maturity")
       (goals
         ("Become authoritative community resource")
         ("Integration with major platforms")
         ("Standards body collaboration"))
       (deliverables
         ("IANA registry synchronization")
         ("Plugins for popular frameworks")
         ("Living standards documentation")
         ("Annual .well-known ecosystem reports"))))

    (stretch-goals
      ((ideas
         ("Browser extension to inspect .well-known files")
         ("Monitoring service for .well-known endpoints")
         ("Best practices working group")
         ("Certification program for implementations"))))))

;;;============================================================================
;;; CRITICAL NEXT ACTIONS
;;;============================================================================

(define critical-next-actions
  '((action-1
      (priority . 1)
      (task . "Owner defines project scope and purpose")
      (active-form . "Defining project scope")
      (blocker . #t)
      (owner . "hyperpolymath"))

    (action-2
      (priority . 2)
      (task . "Create README.md with mission statement")
      (active-form . "Creating README with mission")
      (depends-on . "action-1"))

    (action-3
      (priority . 3)
      (task . "Add LICENSE file")
      (active-form . "Adding license")
      (depends-on . "action-1"))

    (action-4
      (priority . 4)
      (task . "Create index.md for GitHub Pages")
      (active-form . "Creating landing page")
      (depends-on . "action-2"))

    (action-5
      (priority . 5)
      (task . "Fix dependabot.yml package-ecosystem")
      (active-form . "Fixing Dependabot config")
      (file . ".github/dependabot.yml"))))

;;;============================================================================
;;; SESSION CONTEXT
;;;============================================================================

(define session-context
  '((session-id . "create-state-scm-01RYRFvu4SrZBsGcwgZQzW8Q")
    (created-by . "claude-opus-4")
    (purpose . "Initial project state capture and roadmap definition")))

;;;============================================================================
;;; EXPORT STATE
;;;============================================================================

(define state
  `((metadata . ,metadata)
    (current-position . ,current-position)
    (mvp-route . ,mvp-v1-route)
    (issues . ,current-issues)
    (questions . ,questions-for-owner)
    (roadmap . ,long-term-roadmap)
    (next-actions . ,critical-next-actions)
    (session . ,session-context)))

;;; End of STATE.scm
