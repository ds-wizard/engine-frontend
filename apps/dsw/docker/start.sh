#!/bin/sh

# create config: the API is at <origin>/api unless API_URL says otherwise
config=/usr/share/nginx/html/config.js
fields=""
add() { [ -n "$2" ] && fields="${fields:+$fields,}$1:'$2'"; }
add apiUrl "$API_URL"
echo -n "window.app={${fields}}" > ${config}

# head extras: a file mounted at /src/head-extra.html goes into the page just
# before </head>, as it is. The page is made again from the one built into the
# image on every start, so a restart does not add it twice and an unmounted
# file leaves nothing behind.
index=/usr/share/nginx/html/index.html
headExtra=/src/head-extra.html
if [ -f "$headExtra" ]; then
  awk -v extra="$headExtra" '
    !done && (at = index($0, "</head>")) {
      printf "%s", substr($0, 1, at - 1)
      while ((getline line < extra) > 0) print line
      print substr($0, at)
      done = 1
      next
    }
    { print }
  ' /template/index.html > "$index"
else
  cp /template/index.html "$index"
fi

# start nginx
nginx -g 'daemon off;'
