#!/bin/bash
git ls-remote --quiet --exit-code origin gh-pages
if [ $? -ne 0 ]; then
  dt=$(git rev-parse --abbrev-ref HEAD)
  dt=${dt//$'\n'/}
  git checkout --orphan gh-pages
  git rm -rf --quiet .
  git commit --allow-empty -m "Initializing gh-pages branch"
  git push origin HEAD:gh-pages
  git checkout $dt
fi


