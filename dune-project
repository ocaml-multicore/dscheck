(lang dune 2.9)

(generate_opam_files true)

(name dscheck)
(source (github sadiqj/dscheck))
(authors "Sadiq Jaffer")
(maintainers "Sadiq Jaffer")

(package
 (name dscheck)
 (synopsis "Traced Atomics")
 (depends (ocaml (>= 5.0.0)) dune containers oseq))

