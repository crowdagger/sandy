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
  (dependencies `())
  (skip ())
  (features
    ((guix #f)
     (use-guix-specs-for-dependencies #f)
     (native-language-support #f)
     (licensing #f)))
  (files (libraries
           ((scheme-file "sandy")
            (directory
              "sandy"
              ((scheme-file "sandbox") (scheme-file "hconfig")))))
         (tests ((directory "tests" ((scheme-file "sandbox")))))
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
