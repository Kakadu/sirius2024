  $ . "$TESTDIR"/setup.sh

  $ python3 main.py tasks/2loops/2.lama.json out.json -lang loops
$ ls
$ cat out.json
  $ OCAMLRUNPARAM=b lama_bc_json out.json
  Result: 5
