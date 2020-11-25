(lang dune 2.7)
(name ReqCheck)

(generate_opam_files true)
(source (github rosalogia/ReqCheck))
(authors "Ambika Eshwar")
(maintainers "ambika.eshwar@rutgers.edu")

(package
 (name ReqCheck)
 (synopsis "A personalised course pre-requisite identifying program")
 (description "A flexible command-line program to determine which university courses are or will be available to you based on which courses you've already taken")
 (depends
  (cohttp (>= 2.5.4))
  (yojson (>= 1.7.0))
  (angstrom (>= 0.15.0))))

