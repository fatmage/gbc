(lang dune 3.14)

(name gbc)

(generate_opam_files true)

(source
 (github fatmage/gbc))

(authors "Michał Bednarz <mi.bed2001@gmail.com>")

(maintainers "Michał Bednarz <mi.bed2001@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name gbc)
 (synopsis "GameBoy Caml")
 (description "GameBoy Color emulator written in OCaml")
 (depends ocaml dune)
 (tags
  (emulator gameboy gbc gb console debugger)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
