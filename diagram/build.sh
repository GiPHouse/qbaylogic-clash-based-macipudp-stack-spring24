#!/usr/bin/env sh
set -euxo pipefail

for i in *.puml
do
  plantuml -tsvg "$i"
  plantuml -tpng "$i"
done
