(lang dune 3.19)
(using ctypes 0.3)

(name mlirml)

(generate_opam_files true)

(source
 (github watch4d0gg/mlirml))

(authors "Matej Tomášek <tomasek.matej@gmail.com>")

(maintainers "Matej Tomášek <tomasek.matej@gmail.com>")

; (license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name mlirml)
 (synopsis "OCaml bindings to MLIR C API")
;  (description "A longer description")
 (depends ocaml ctypes ctypes-foreign)
 (tags
  (llvm mlir bindings)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
