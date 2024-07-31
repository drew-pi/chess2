# Chess 2

## How to run

Follow the following [instructions](https://cs3110.github.io/textbook/chapters/preface/install.html) to download and configure ocaml (est. time 5 mins)

Then clone this repository by running 
```
git clone git@github.com:drew-pi/chess2.git
```

Navigate into the repository 
```
cd chess2
```

Download all of the required dependencies 
```
opam install integers ounit2
```

Finally, compile and run the game 
```
dune build && dune exec bin/main.exe
```

The onscreen instructions will guide you through the rest :)

## Run test suite
#### [Source](https://github.com/aantron/bisect_ppx?tab=readme-ov-file)

To run the test suite with bisect (a code coverage monitoring tool) use
```
find . -name '*.coverage' | xargs rm -f
dune runtest --instrument-with bisect_ppx --force
```

Then to generate the coverage report run in ```_coverage/index.html``` run 
```
bisect-ppx-report html
```

You can also generate a short summary by using 
```
bisect-ppx-report summary
```

---

To run the test suite without bisect use 
```
dune runtest
```
