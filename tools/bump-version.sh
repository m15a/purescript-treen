#!/usr/bin/env bash

set -euo pipefail

BUMP_STRATEGY=bugfix
for option in "$@"; do
    case "$option" in
        --major|-M)
            BUMP_STRATEGY=major
            ;;
        --minor|-m)
            BUMP_STRATEGY=minor
            ;;
        --bugfix|-b)
            BUMP_STRATEGY=bugfix
            ;;
    esac
done

CURRENT_VERSION="$(spago run -- --version 2>/dev/null)"
CURRENT_MAJOR="$(echo -n "$CURRENT_VERSION" | cut -d. -f1)"
CURRENT_MINOR="$(echo -n "$CURRENT_VERSION" | cut -d. -f2)"
CURRENT_BUGFIX="$(echo -n "$CURRENT_VERSION" | cut -d. -f3)"

case "$BUMP_STRATEGY" in
    major)
        NEXT_VERSION="$(("$CURRENT_MAJOR" + 1)).0.0"
        ;;
    minor)
        NEXT_VERSION="$CURRENT_MAJOR.$(("$CURRENT_MINOR" + 1)).0"
        ;;
    bugfix)
        NEXT_VERSION="$CURRENT_MAJOR.$CURRENT_MINOR.$(("$CURRENT_BUGFIX" + 1))"
        ;;
esac

sed -i -E spago.yaml -e "s|(version: )$CURRENT_VERSION|\1$NEXT_VERSION|"
sed -i -E package.json -e "s|(\"version\": \")$CURRENT_VERSION(\")|\1$NEXT_VERSION\2|"
sed -i -E package.nix -e "s|(version = \")$CURRENT_VERSION(\")|\1$NEXT_VERSION\2|"
sed -i -E src/Treen/App/Version.purs -e "s|(ersion = \")$CURRENT_VERSION(\")|\1$NEXT_VERSION\2|"

git add spago.yaml package.json package.nix src/Treen/App/Version.purs
git commit -m "release: bump version $CURRENT_VERSION -> $NEXT_VERSION"
