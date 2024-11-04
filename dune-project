(lang dune 3.16)

(name swapfs)

(generate_opam_files true)

(source
 (github robur-coop/mirage-swapfs))

(authors "Reynir Björnsson <reynir@reynir.dk>")

(maintainers "Robur Team <team@robur.coop>")

(license MIT)

(documentation https://robur-coop.github.io/mirage-swapfs/doc)

(package
 (name swapfs)
 (synopsis "Streaming temporary storage for Mirage OS")
 (description
   "\| Append-only ephemeral, anonymous files.
   "\| Swapfs is useful for temporarily putting large data on persistent storage,
   "\| for example if the data is too large to comfortably sit in memory.
   "\| The data can then be read back in chunks.
   )
 (depends ocaml dune mirage-block lwt logs)
 (tags
  (topics org:mirage org:robur swap)))
