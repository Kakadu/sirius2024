  $ . "$TESTDIR"/setup.sh

  $ python3 main.py tasks/1expr/1.lama.json out.json -lang expr
$ ls
$ cat out.json
  $ lama_bc_json out.json  
  Result: 1

