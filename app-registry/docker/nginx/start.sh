#!/bin/sh

# create config
config=/usr/share/nginx/html/config.js
echo -n "window.app={apiUrl:'"$API_URL"'" > ${config}

if [[ ! -z "$APP_TITLE" ]]; then
  echo -n ",appTitle:'"$APP_TITLE"'" >>${config}
fi

echo "};" >> ${config}

nginx -g 'daemon off;'
