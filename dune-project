(lang dune 3.16)

(name swapfs)

(generate_opam_files true)

(source
 (github reynir/swapfs))

(authors "Reynir Björnsson <reynir@reynir.dk>")

(maintainers "Reynir Björnsson <reynir@reynir.dk>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name swapfs)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune mirage-block)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
