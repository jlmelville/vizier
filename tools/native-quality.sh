#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
clangxx=${CLANGXX:-clang++}
formatter=${CLANG_FORMAT:-clang-format}
tidy=${CLANG_TIDY:-clang-tidy}
if [[ ! $("$formatter" --version) =~ version\ 21\.1\.8([^0-9.]|$) ]]; then
  echo 'Native formatting requires clang-format 21.1.8.' >&2
  exit 1
fi

"$formatter" --dry-run --Werror src/hammer.cpp src/hammer.h tools/check-hammer.cpp
includes=$(Rscript --vanilla -e '
  cat(c(R.home("include"), system.file("include", package = "Rcpp", mustWork = TRUE)), sep = "\n")
')
flags=(-std=c++17 -pthread -fPIC -DRCPP_NO_RTTI -DSTRICT_R_HEADERS -DRCPP_NO_MODULES -Isrc)
while IFS= read -r path; do
  flags+=(-isystem "$path")
done <<< "$includes"
warnings=(-Wall -Wextra -Wpedantic -Wformat=2 -Wnull-dereference -Werror)
for compiler in g++ "$clangxx"; do
  "$compiler" "${flags[@]}" "${warnings[@]}" -fsyntax-only src/hammer.cpp tools/check-hammer.cpp
done
"$tidy" --quiet --warnings-as-errors='*' \
  --checks='-*,clang-analyzer-core.*,bugprone-unused-raii,bugprone-use-after-move,performance-unnecessary-copy-initialization' \
  --header-filter='(^|/)src/' src/hammer.cpp tools/check-hammer.cpp -- "${flags[@]}"
