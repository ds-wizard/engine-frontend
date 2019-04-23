#!/bin/sh

# create config
config=/usr/share/nginx/html/config.js
echo -n "window.dsw={apiUrl:'"$API_URL"'," > ${config}

if [[ ! -z "$APP_TITLE" ]]; then
    echo -n "appTitle:'"$APP_TITLE"'," >> ${config}
fi

if [[ ! -z "$APP_TITLE_SHORT" ]]; then
    echo -n "appTitleShort:'"$APP_TITLE_SHORT"'," >> ${config}
fi

if [[ ! -z "$WELCOME_WARNING" ]]; then
    echo -n "welcomeWarning:'"$WELCOME_WARNING"'," >> ${config}
fi

if [[ ! -z "$WELCOME_INFO" ]]; then
    echo -n "welcomeInfo:'"$WELCOME_INFO"'," >> ${config}
fi

echo -n "};" >> ${config}


# regenerate styles when customizations exist
if [[ -f /customizations/variables.scss ]]; then
    cat /customizations/variables.scss > /src/scss/_variables.new.scss
    echo '$fa-font-path: "";' >> /src/scss/_variables.new.scss
    cat /src/scss/_variables.scss >> /src/scss/_variables.new.scss
    rm /src/scss/_variables.scss
    mv /src/scss/_variables.new.scss /src/scss/_variables.scss
    find /usr/share/nginx/html -name "*.css" -exec sassc -I /src -t compressed /src/scss/main.scss {} \;
fi


nginx -g 'daemon off;'
