(lang dune 2.5)

(using menhir 2.1)

(name orient)

(package
 (name orient)
 (depends
  (ocaml
   (>= 4.14))
  (menhir
   (>= 2.0))
  (core
   (>= 0.17))
  (core_unix
   (>= 0.15))))
