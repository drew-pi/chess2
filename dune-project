(lang dune 3.14)

(name chess2)

(generate_opam_files true)

(source
 (github drew-pi/chess2))

(authors "Andrew Piatetsky")

(maintainers "Andrew Piatetsky")

(license LICENSE)

(package
 (name chess2)
 (synopsis "a chess game in OCaml featuring a versatile bot with multiple difficulty levels, supporting both single-player and two-player modes. The game offers a choice between a terminal-based interface and a graphical user interface (GUI) for an engaging user experience.")
 (description "A fully-featured chess game in OCaml, incorporating both single-player and two-player gameplay modes. The game includes a sophisticated AI bot capable of playing at multiple difficulty levels, challenging players of varying skill levels. Users can choose between a traditional terminal interface or a more visually appealing graphical user interface (GUI), providing flexibility in how they interact with the game. The project showcases advanced OCaml programming skills, including algorithm design, user interface development, and AI integration, delivering an immersive and customizable chess-playing experience.")
 (depends ocaml dune)
 (tags
  (topics "Chess ML Multiplayer Singleplayer GUI" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
