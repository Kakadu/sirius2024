```
inotifywait -mr -e close_write tasks main.py hardcoded.py expr.t | while read base event file
do
  grc -c conf.diff cram ./expr.t
done
```