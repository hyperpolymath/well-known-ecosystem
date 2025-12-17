;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; ECOSYSTEM.scm — well-known-ecosystem

(ecosystem
  (version "1.0.0")
  (name "well-known-ecosystem")
  (type "project")
  (purpose "Well-known URI ecosystem management with RSR compliance enforcement")

  (position-in-ecosystem
    "Part of hyperpolymath ecosystem. Follows RSR guidelines.")

  (related-projects
    (project (name "rhodium-standard-repositories")
             (url "https://github.com/hyperpolymath/rhodium-standard-repositories")
             (relationship "standard")))

  (what-this-is "A Guix channel providing .well-known URI infrastructure and RSR reference implementation")
  (what-this-is-not "- NOT exempt from RSR compliance"))
