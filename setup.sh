cp -R "$TESTDIR"/tasks .
cp "$TESTDIR"/*.py .
find . -name '*.err' -exec rm '{}' \;