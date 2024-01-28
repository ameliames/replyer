(lang dune 3.12)

(name replyer)

(generate_opam_files true)

(source
 (github ameliames/replyer))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name replyer)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune omd cohttp-lwt cohttp-lwt-unix)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
