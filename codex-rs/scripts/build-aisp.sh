#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
BIN_DIR="${HOME}/.local/bin"
TARGET_BIN="${REPO_DIR}/target/release/codex"
LINK_PATH="${BIN_DIR}/aisp-codex"

cd "${REPO_DIR}"
cargo build -p codex-cli --release

if [[ ! -x "${TARGET_BIN}" ]]; then
  echo "error: expected built binary not found: ${TARGET_BIN}" >&2
  exit 1
fi

mkdir -p "${BIN_DIR}"
ln -sf "${TARGET_BIN}" "${LINK_PATH}"

echo "Linked ${LINK_PATH} -> ${TARGET_BIN}"
