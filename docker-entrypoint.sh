#!/bin/sh
set -eu

if [ -z "${SHINY_SIMPLE_SCHEDULER+x}" ]; then SHINY_SIMPLE_SCHEDULER=30; fi
if [ -z "${SHINY_APP_IDLE_TIMEOUT+x}" ]; then SHINY_APP_IDLE_TIMEOUT=60; fi
if [ -z "${SHINY_APP_SESSION_TIMEOUT+x}" ]; then SHINY_APP_SESSION_TIMEOUT=0; fi

if [ -n "${SHINY_APP_SESSION_TIMEOUT}" ] && [ "${SHINY_APP_SESSION_TIMEOUT}" != "0" ]; then
  APP_SESSION_TIMEOUT_LINE="$(printf '    # SHINY_APP_SESSION_TIMEOUT=%s requested, but Shiny Server v1.5.x does not support app_session_timeout; omitted.' "${SHINY_APP_SESSION_TIMEOUT}")"
else
  APP_SESSION_TIMEOUT_LINE="    # app_session_timeout omitted because SHINY_APP_SESSION_TIMEOUT is blank or 0."
fi

export SHINY_SIMPLE_SCHEDULER SHINY_APP_IDLE_TIMEOUT APP_SESSION_TIMEOUT_LINE
RENDERED_CONF="${SHINY_SERVER_CONF:-/tmp/shiny-server.conf}"

mkdir -p /srv/shiny-server/appdata/data /var/log/shiny-server
chown -R shiny:shiny /srv/shiny-server/appdata /var/log/shiny-server 2>/dev/null || true

awk '
{
  gsub(/\$\{SHINY_SIMPLE_SCHEDULER\}/, ENVIRON["SHINY_SIMPLE_SCHEDULER"]);
  gsub(/\$\{SHINY_APP_IDLE_TIMEOUT\}/, ENVIRON["SHINY_APP_IDLE_TIMEOUT"]);
  gsub(/\$\{APP_SESSION_TIMEOUT_LINE\}/, ENVIRON["APP_SESSION_TIMEOUT_LINE"]);
  print;
}
' /etc/shiny-server/shiny-server.conf.template > "${RENDERED_CONF}"

if [ "$#" -gt 0 ] && [ "$1" != "/usr/bin/shiny-server" ] && [ "$1" != "shiny-server" ]; then
  exec "$@"
fi

exec /usr/bin/shiny-server "${RENDERED_CONF}"
