#!/bin/sh
# Builds both editions and the edition chooser into a static site.
#
#   ./build.sh [output-directory]    (default: dist)
#
# Requires elm-make 0.18 on the PATH. The result can be served from any path
# prefix: each edition works out its base path from the URL of its main.js.
set -eu

ROOT=$(cd "$(dirname "$0")" && pwd)
OUT=${1:-dist}
mkdir -p "$OUT"
OUT=$(cd "$OUT" && pwd)

# Writes the edition's index.html template to <dir>/index.html, pointing
# asset URLs at the edition root (<root> is "", "../" or "../../").
page() {
    template=$1 dir=$2 root=$3
    mkdir -p "$dir"
    sed "s#{{root}}#$root#g" "$template" > "$dir/index.html"
}

for edition in classic algebra; do
    src="$ROOT/$edition"
    dest="$OUT/$edition"
    rm -rf "$dest"
    mkdir -p "$dest"

    (cd "$src" && elm-make --yes --output "$dest/elm.js" src/Main.elm)
    cat "$dest/elm.js" "$src/glyphs.js" > "$dest/main.js"
    rm "$dest/elm.js"
    cp "$src/site/main.css" "$src/site/icon.png" "$dest/"

    # The app routes on the URL path, so every route needs an index.html
    # that loads the app.
    template="$src/site/index.html"
    page "$template" "$dest" ""
    page "$template" "$dest/about" "../"
    if [ "$edition" = algebra ]; then
        page "$template" "$dest/introduction" "../"
    fi
    count=$(grep -c '^ *[[,] { description = ' "$src/src/Problems.elm")
    i=1
    while [ "$i" -le "$count" ]; do
        page "$template" "$dest/problem-$i" "../"
        page "$template" "$dest/problem-$i/proof" "../../"
        i=$((i + 1))
    done
    echo "$edition: $count problems"
done

cp "$ROOT/site/index.html" "$ROOT/site/icon.png" "$OUT/"
echo "Site written to $OUT"
