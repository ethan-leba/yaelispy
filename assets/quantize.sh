#!/usr/bin/env bash
set -euo pipefail

~/go/bin/asciinema-edit speed --factor 10 $1 > foo.swp
~/go/bin/asciinema-edit quantize --range 0.4 foo.swp > $1
rm foo.swp
