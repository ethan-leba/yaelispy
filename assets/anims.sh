#!/usr/bin/env bash
set -euo pipefail

~/go/bin/asciinema-edit speed --factor 10 movement.cast > movement-processed.cast
~/go/bin/asciinema-edit quantize --range 0.4 movement-processed.cast > movement.cast

svg-term --from 11500 --to 17000 --width 33 --height 1 < movement2.cast > lispy-flow.svg
svg-term --from 16500 --to 19000 --width 33 --height 1 < movement2.cast > lispy-up.svg
