(lang dune 3.17)

(using menhir 3.0)

(name colang)

(generate_opam_files true)

(source
 (github invrtd-h/colang2))

(authors "invrtd-h")

(maintainers "invrtd-h")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name colang)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (ocaml (>= 5.4.0))
  dune
  (core (>= 0.17.0))
  (climate (>= 0.9.0))
  sedlex)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
