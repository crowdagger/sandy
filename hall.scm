(hall-description
  (name "sandy")
  (prefix "")
  (version "0.1")
  (author "Lizzie Crowdagger")
  (email "")
  (copyright (2025))
  (synopsis "")
  (description "")
  (home-page "")
  (license gpl3+)
  (dependencies ())
  (skip ())
  (features
    ((guix #t)
     (use-guix-specs-for-dependencies #t)
     (native-language-support #f)
     (licensing #f)))
  (files (libraries
           ((scheme-file "sandy")
            (directory
              "sandy"
              ((scheme-file "grid") (scheme-file "hconfig")))
            (directory
             "crow-utils"
             ((scheme-file "checked")))))
         (tests ((directory "tests" ((scheme-file "grid")))))
         (programs
           ((directory "scripts" ((scheme-file "sandy")))))
         (documentation
           ((org-file "README")
            (symlink "README" "README.org")
            (text-file "HACKING")
            (text-file "COPYING")
            (directory "doc" ((texi-file "sandy")))))
         (infrastructure
           ((scheme-file "guix") (scheme-file "hall")))))
