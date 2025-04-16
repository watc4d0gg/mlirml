(lang dune 3.18)

(name ocaml-mlir-bindings)

(generate_opam_files true)

(source
 (github watch4d0gg/ocaml-mlir-bindings))

(authors "Matej Tomášek <tomasek.matej@gmail.com>")

; (maintainers "Maintainer Name <maintainer@example.com>")

; (license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name ocaml-mlir-bindings)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml (ctypes (= stubs)))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
