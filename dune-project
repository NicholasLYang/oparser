(lang dune 3.17)

(name oparser)

(generate_opam_files true)

(source
 (github nicholaslyang/reponame))

(authors "Nicholas Yang <nick@nicholasyang.com>")

(maintainers "Nicholas Yang <nick@nicholasyang.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name oparser)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml grace sedlex)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
