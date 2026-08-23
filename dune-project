(lang dune 3.23)

(name ulc)

(generate_opam_files true)

(source
 (github sweiglbosker/ulc))

(authors "Stefan Weigl-Bosker <stefan@s00.xyz>")

(license LICENSE)

(documentation https://github.com/sweiglbosker/ulc)

(package
 (name ulc)
 (synopsis "ulc")
 (depends
   ocaml
   (alcotest :with-test)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
