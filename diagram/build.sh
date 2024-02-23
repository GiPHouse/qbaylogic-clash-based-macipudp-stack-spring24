#!/usr/bin/env sh
set -euxo pipefail

plantuml -tsvg layer1+2-arch.puml
plantuml -tsvg uart-eth.puml
