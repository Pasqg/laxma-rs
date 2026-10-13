#!/bin/bash
set -e

LAXMA_LIB="${LAXMA_LIB:-/usr/local/share/laxma}"

cargo build --release

echo "laxma requires administrator privileges to install to /usr/local/bin/laxma"
sudo cp target/release/laxma-rs /usr/local/bin/laxma

# Installs the standard library, which the binary searches for imports
sudo mkdir -p "$LAXMA_LIB"
sudo cp -r lib "$LAXMA_LIB/"

if [ "$LAXMA_LIB" != "/usr/local/share/laxma" ]; then
  case "$SHELL" in
    *zsh*) PROFILE="$HOME/.zshrc" ;;
    *)     PROFILE="$HOME/.bashrc" ;;
  esac
  LINE="export LAXMA_LIB=\"$LAXMA_LIB\""
  touch "$PROFILE"
  grep -qxF "$LINE" "$PROFILE" || echo "$LINE" >> "$PROFILE"
  echo "exported LAXMA_LIB in $PROFILE (start a new shell or run: source $PROFILE)"
fi

echo "laxma installed to /usr/local/bin/laxma"
echo "stdlib installed to $LAXMA_LIB"
