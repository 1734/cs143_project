find test -type f -name "test*.cl" | xargs -I{} ./diff.sh "{}"
