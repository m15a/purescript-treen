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

CURRENT="$(spago run -- --version 2>/dev/null)"
MAJOR="$(echo -n "$CURRENT" | cut -d. -f1)"
MINOR="$(echo -n "$CURRENT" | cut -d. -f2)"
BUGFIX="$(echo -n "$CURRENT" | cut -d. -f3)"

case "$BUMP_STRATEGY" in
    major)
        NEXT="$(("$MAJOR" + 1)).$MINOR.$BUGFIX"
        ;;
    minor)
        NEXT="$MAJOR.$(("$MINOR" + 1)).$BUGFIX"
        ;;
    bugfix)
        NEXT="$MAJOR.$MINOR.$(("$BUGFIX" + 1))"
        ;;
esac

sed -i -E spago.yaml -e "s|(version: )$CURRENT|\1$NEXT|"
sed -i -E src/Treen/App/Version.purs -e "s|(ersion = \")$CURRENT(\")|\1$NEXT\2|"

git add spago.yaml src/Treen/App/Version.purs
git commit -m "release: bump version $CURRENT -> $NEXT"
