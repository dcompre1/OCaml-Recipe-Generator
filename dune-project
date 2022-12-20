(lang dune 3.4)
(generate_opam_files true)
(authors "Dinamary Compres")
(maintainers "dcompre1@jh.edu")
(package
 (name project) ; put in your own project name here
 (synopsis "An OCaml program for recipe generation")
 (description "Recipe Generator with many command prompts and interaction with TheMealDBAPI")
 (depends
  (ounit2 (> 2.2.6)) ; for each opam dependency list the version
  (core (> 0.15.0)) ; `opam list core` will display which version of core you have
  (cohttp (> 5.0.0)) 
  (core_unix (> 0.15.0))
  (stdio (> 0.15.0))
))
