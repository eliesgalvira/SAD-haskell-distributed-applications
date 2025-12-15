#!/usr/bin/env bash
cd "$(dirname "$0")/.."
export HLS_WRAPPER=1
exec nix develop --quiet -c haskell-language-server-wrapper "$@"
