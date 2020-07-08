#!/bin/sh


# load defaults from profile
file=/usr/share/nginx/html/main.*.js
source /configuration/profile.sh
sed -i "s#{defaultAppTitle}#$DEFAULT_APP_TITLE#g" $file
sed -i "s#{defaultAppTitleShort}#$DEFAULT_APP_TITLE_SHORT#g" $file
sed -i "s#{defaultPrivacyUrl}#$DEFAULT_PRIVACY_URL#g" $file
sed -i "s#{defaultTermsOfServiceUrl}#$DEFAULT_TERMS_OF_SERVICE_URL#g" $file
sed -i "s#{defaultSupportEmail}#$DEFAULT_SUPPORT_EMAIL#g" $file
sed -i "s#{defaultSupportRepositoryName}#$DEFAULT_SUPPORT_REPOSITORY_NAME#g" $file
sed -i "s#{defaultSupportRepositoryUrl}#$DEFAULT_SUPPORT_REPOSITORY_URL#g" $file


# create config
config=/usr/share/nginx/html/config.js
echo -n "window.wizardResearch={apiUrl:'"$API_URL"'" > ${config}
if [[ ! -z "$PROVISIONING_URL" ]]; then
  echo -n ",provisioningUrl:'"$PROVISIONING_URL"'" >> ${config}
fi
provisioning=/configuration/provisioning.json
if [[ -f "$provisioning" ]]; then
  echo -n ",provisioning:" >> ${config}
  cat $provisioning >> ${config}
fi
echo -n "};" >> ${config}


# check if customizations exist
if [[ $(find /src/scss/customizations -name "*.scss" | xargs cat | wc -l) -gt 0 ]]; then
  # regenerate styles
  echo '$fa-font-path: "";' >> /src/scss/customizations/_variables-app.scss
  find /usr/share/nginx/html -name "*.css" -exec sassc -I /src -t compressed /src/scss/main.scss {} \;

  # replace primary color in illustrations if defined
  appIllustrationsColor=$(sed -ne "s/\\\$illustrations-color: *#\([0-9a-fA-F]*\);/\1/p" /src/scss/customizations/_variables-app.scss)
  customIllustrationsColor=$(sed -ne "s/\\\$illustrations-color: *#\([0-9a-fA-F]*\);/\1/p" /src/scss/customizations/_variables.scss)
  illustrationsColor=${customIllustrationsColor:=$appIllustrationsColor}

  if [[ ! -z "$illustrationsColor" ]]; then
    defaultIllustrationsColor="4285f4"
    find /usr/share/nginx/html/img/illustrations -name "*.svg" | xargs sed -i -e 's/'"$defaultIllustrationsColor"'/'"$illustrationsColor"'/g'
  fi
fi


# start nginx
nginx -g 'daemon off;'
