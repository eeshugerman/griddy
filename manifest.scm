;;; GUIX_PROFILE=~/.guix-extra-profiles/griddy
;;; guix pull -C channels-spec.scm && guix package -u
;;; guix package -m manifest.scm

(specifications->manifest
 '("guile@3.0.7"
   "guile-chickadee@0.8.0"))
