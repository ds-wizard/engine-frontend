#!/bin/sh

# load defaults from profile
file=/usr/share/nginx/html/wizard/main.*.js
source /configuration/profile.sh
sed -i "s#{defaultAppTitle}#$DEFAULT_APP_TITLE#g" $file
sed -i "s#{defaultAppTitleShort}#$DEFAULT_APP_TITLE_SHORT#g" $file
sed -i "s#{defaultSupportEmail}#$DEFAULT_SUPPORT_EMAIL#g" $file
sed -i "s#{defaultSupportRepositoryName}#$DEFAULT_SUPPORT_REPOSITORY_NAME#g" $file
sed -i "s#{defaultSupportRepositoryUrl}#$DEFAULT_SUPPORT_REPOSITORY_URL#g" $file
sed -i "s/{defaultPrimaryColor}/$DEFAULT_PRIMARY_COLOR/g" $file
sed -i "s/{defaultIllustrationsColor}/$DEFAULT_ILLUSTRATIONS_COLOR/g" $file

# create config
config=/usr/share/nginx/html/wizard/config.js
echo -n "window.app={apiUrl:'"$API_URL"'" >${config}
if [[ ! -z "$PROVISIONING_URL" ]]; then
  echo -n ",provisioningUrl:'"$PROVISIONING_URL"'" >>${config}
fi
if [[ ! -z "$GA_ID" ]]; then
  echo -n ",gaID:'"$GA_ID"'" >>${config}
fi
provisioning=/configuration/provisioning.json
if [[ -f "$provisioning" ]]; then
  echo -n ",provisioning:" >>${config}
  cat $provisioning >>${config}
fi
echo -n "};" >>${config}

# check for custom html head block
headExtraFile=/src/head-extra.html
if [ -f "$headExtraFile" ]; then
  headExtraBlock=$(cat $headExtraFile)
  sed -i "s~</head>~$headExtraBlock</head>~g" /usr/share/nginx/html/wizard/index.html
fi

# start nginx
nginx -g 'daemon off;'
