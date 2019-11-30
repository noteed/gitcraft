#! /usr/bin/env bash

set -e

# Run Pandoc on source files using the design-system template.
for i in \
  index \
  ; do \
  echo Rendering $i.md...
nix-shell -p pkgs.haskellPackages.pandoc --run "pandoc \
  --standalone \
  --template ../design-system/pandoc/default.html \
  --lua-filter ../design-system/pandoc/tachyons.lua \
  --output docs/$i.html \
  $i.md"

  # TODO noteed.com should have all these files already.
  sed -i -e 's@/static/@./static/@g' docs/$i.html
done

cp ../design-system/static/css/styles.css docs/static/css/
cp ../design-system/docs/static/css/tachyons.min.v4.11.1.css docs/static/css/
cp ../design-system/docs/static/css/inter.css docs/static/css/
cp ../design-system/docs/static/css/style.css docs/static/css/
