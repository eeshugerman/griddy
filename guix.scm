;; guix environment --load=guix.scm

;; > guix describe
;; guix a0178d3
;; repository URL: https://git.savannah.gnu.org/git/guix.git
;; branch: master
;; commit: a0178d34f582b50e9bdbb0403943129ae5b560ff

(use-modules (guix gexp)
             (guix packages)
             (guix build-system guile)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages game-development))
(package
 (name "griddy")
 (version "0.0.1")
 (source (local-file (dirname (current-filename))))
 (build-system guile-build-system)
 (synopsis "")
 (description "")
 (license "")
 (home-page "")
 (inputs `(("guile" ,guile-3.0)
           ("chickadee" ,guile-chickadee))))
