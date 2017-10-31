#!/bin/sh
sed -i -e 's@http://localhost:3000@'"$API_URL"'@g' /usr/share/nginx/html/app.js
nginx -g 'daemon off;'
