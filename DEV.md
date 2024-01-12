#### Sending a JSON bytecode to Web form

Use curl or ask.py

    curl --json '[ { kind: "IMPORT", value: "Std" }, { kind: "EXTERN", value: "Lwrite" }, { kind: "CONST", value: 42 }, { kind: "CALL", fname: "Lwrite", argc: 1, "flg": false } ]' http://localhost:8040/eval



#### Testing cram tests on save

```
inotifywait -mr -e close_write tasks main.py hardcoded.py expr.t | while read base event file
do
  grc -c conf.diff cram ./expr.t
done
```