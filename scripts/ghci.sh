#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghci \
  -isrc \
  -ghci-script scripts/ghci.conf \
  bin/reduced.hs
