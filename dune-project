(lang dune 3.14)

(name chess2)

(generate_opam_files true)

(source
 (github drew-pi/chess2))

(authors "Andrew Piatetsky")

(maintainers "Andrew Piatetsky")

(license LICENSE)

(package
 (name chess2)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
