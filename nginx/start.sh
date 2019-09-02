#!/bin/sh

# create config
config=/usr/share/nginx/html/config.js
echo -n "window.dsw={apiUrl:'"$API_URL"'" > ${config}
if [[ ! -z "$PROVISIONING_URL" ]]; then
    echo -n ",provisioningUrl:'"$PROVISIONING_URL"'" >> ${config}
fi
echo -n "};" >> ${config}


# check if customizations exist
if [[ -f /customizations/variables.scss ]]; then
    # regenerate styles
    cat /customizations/variables.scss > /src/scss/_variables.new.scss
    echo '$fa-font-path: "";' >> /src/scss/_variables.new.scss
    cat /src/scss/_variables.scss >> /src/scss/_variables.new.scss
    rm /src/scss/_variables.scss
    mv /src/scss/_variables.new.scss /src/scss/_variables.scss
    find /usr/share/nginx/html -name "*.css" -exec sassc -I /src -t compressed /src/scss/main.scss {} \;

    # replace primary color in illustrations
    primary=$(sed -ne "s/\\\$primary: *#\([0-9a-fA-F]*\);/\1/p" /customizations/variables.scss)

    if [[ ! -z "$primary" ]]; then
        defaultPrimary="ff6347"
        find /usr/share/nginx/html/img/illustrations -name "*.svg" | xargs sed -i -e 's/'"$defaultPrimary"'/'"$primary"'/g'
    fi
fi


nginx -g 'daemon off;'
