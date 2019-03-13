#!/bin/sh
sed -i -e 's@http://localhost:3000@'"$API_URL"'@g' /usr/share/nginx/html/*.js

if [[ -f /customizations/variables.scss ]]; then
    cat /customizations/variables.scss > /src/scss/_variables.new.scss
    cat /src/scss/_variables.scss >> /src/scss/_variables.new.scss
    rm /src/scss/_variables.scss
    mv /src/scss/_variables.new.scss /src/scss/_variables.scss
    find /usr/share/nginx/html -name "*.css" -exec sassc -I /src -t compressed /src/scss/main.scss {} \;
fi

nginx -g 'daemon off;'
