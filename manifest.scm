;;; GUIX_PROFILE=~/.guix-extra-profiles/griddy
;;; guix pull -C channels-spec.scm && guix package -u
;;; guix package -m manifest.scm
;;; guix environment -p $GUIX_PROFILE  # can skip to this one after initial setup

(specifications->manifest
 '("guile@3.0.7"
   "guile-chickadee@0.8.0"
   ;; https://hg.sr.ht/~bjoli/guile-threading-macros looks cooler
   ;; but isn't packaged in guix
   "guile-pipe"))
