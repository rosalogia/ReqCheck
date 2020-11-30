(lang dune 2.7)
(name ReqCheck)

(generate_opam_files true)
(source (github rosalogia/ReqCheck))
(authors "Ambika Eshwar")
(maintainers "ambika.eshwar@rutgers.edu")

(package
 (name ReqCheck)
 (synopsis "A Rutgers CS/Math course availability API")
 (description "A light-weight web API to show Rutgers Computer Science students what CS and Math courses they're eligible for in the coming semester")
 (depends
  (yojson   (>= 1.7.0))
  (logs     (>= 0.7.0))
  (angstrom (>= 0.15.0))))

