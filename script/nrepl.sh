#!/usr/bin/env bash -euo pipefail
cd "`dirname $0`/.."

clj -A:profile -M -m codingame.main --interactive