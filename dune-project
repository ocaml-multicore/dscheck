(lang dune 2.9)

(generate_opam_files true)

(name dscheck)
(source (github ocaml-multicore/dscheck))
(license ISC)
(authors "Sadiq Jaffer")
(maintainers "Sadiq Jaffer")

(package
 (name dscheck)
 (synopsis "Traced Atomics")
 (depends (ocaml (>= 5.0.0)) dune containers tsort oseq (alcotest (and (>= 1.6.0) :with-test))))


