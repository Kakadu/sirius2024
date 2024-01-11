  $ . "$TESTDIR"/setup.sh
$ tree tasks 
  $ python3 main.py tasks/1expr/1.lama.json out.json -lang expr
$ ls
$ cat out.json
  $ lama_bc_json out.json  
  Result: 1

  $ python3 main.py tasks/2loops/2.lama.json out.json -lang loops
$ ls
$ cat out.json
  $ OCAMLRUNPARAM=b lama_bc_json out.json
  Result: 5
