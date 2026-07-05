#!/bin/sh
set -eu

if [ -z "${SHINY_SIMPLE_SCHEDULER+x}" ]; then SHINY_SIMPLE_SCHEDULER=30; fi
if [ -z "${SHINY_APP_IDLE_TIMEOUT+x}" ]; then SHINY_APP_IDLE_TIMEOUT=60; fi
if [ -z "${SHINY_APP_SESSION_TIMEOUT+x}" ]; then SHINY_APP_SESSION_TIMEOUT=900; fi

if [ -n "${SHINY_APP_SESSION_TIMEOUT}" ] && [ "${SHINY_APP_SESSION_TIMEOUT}" != "0" ]; then
  APP_SESSION_TIMEOUT_LINE="$(printf '    # Seconds before an inactive browser session is disconnected.\n    app_session_timeout %s;' "${SHINY_APP_SESSION_TIMEOUT}")"
else
  APP_SESSION_TIMEOUT_LINE="    # app_session_timeout omitted because SHINY_APP_SESSION_TIMEOUT is blank or 0."
fi

export SHINY_SIMPLE_SCHEDULER SHINY_APP_IDLE_TIMEOUT APP_SESSION_TIMEOUT_LINE

awk '
{
  gsub(/\$\{SHINY_SIMPLE_SCHEDULER\}/, ENVIRON["SHINY_SIMPLE_SCHEDULER"]);
  gsub(/\$\{SHINY_APP_IDLE_TIMEOUT\}/, ENVIRON["SHINY_APP_IDLE_TIMEOUT"]);
  gsub(/\$\{APP_SESSION_TIMEOUT_LINE\}/, ENVIRON["APP_SESSION_TIMEOUT_LINE"]);
  print;
}
' /etc/shiny-server/shiny-server.conf.template > /etc/shiny-server/shiny-server.conf

if [ "$#" -gt 0 ] && [ "$1" != "/usr/bin/shiny-server" ] && [ "$1" != "shiny-server" ]; then
  exec "$@"
fi

exec /usr/bin/shiny-server
