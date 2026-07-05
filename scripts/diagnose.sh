#!/bin/sh
set -eu

APPDATA="${APPDATA:-./appdata}"
LOGDIR="${SHINY_LOG_DIR:-./shiny-logs}"

echo "== docker stats =="
if command -v docker >/dev/null 2>&1; then
  docker stats --no-stream || true
else
  echo "docker command not found"
fi

echo
echo "== appdata disk usage: ${APPDATA} =="
if [ -e "${APPDATA}" ]; then
  du -sh "${APPDATA}" || true
else
  echo "appdata directory not found"
fi

echo
echo "== recent Shiny logs: ${LOGDIR} =="
if [ -d "${LOGDIR}" ]; then
  find "${LOGDIR}" -maxdepth 1 -type f -print | while IFS= read -r f; do
    echo "-- ${f} --"
    tail -n 80 "${f}" || true
  done
else
  echo "Shiny log directory not found"
fi

echo
echo "== SQLite file sizes =="
if [ -e "${APPDATA}" ]; then
  find "${APPDATA}" -type f \( -name '*.sqlite' -o -name '*.sqlite-wal' -o -name '*.sqlite-shm' -o -name '*.db' \) -exec ls -lh {} \; || true
else
  echo "appdata directory not found"
fi
