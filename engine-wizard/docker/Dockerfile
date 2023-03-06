FROM nginx:alpine

RUN apk --purge del curl

# Install dart-sass
RUN apk add dart-sass=1.58.0-r0 --repository=http://dl-cdn.alpinelinux.org/alpine/edge/testing/

# Dependencies needed to rebuild styles
COPY node_modules/bootstrap src/~bootstrap
COPY node_modules/@fortawesome src/~@fortawesome
COPY node_modules/flatpickr src/~flatpickr

COPY engine-wizard/docker/nginx/default.conf /etc/nginx/conf.d/default.conf
COPY engine-wizard/docker/profile.sh /configuration/profile.sh
COPY engine-wizard/docker/start.sh /start.sh

COPY engine-wizard/scss /src/scss
COPY dist/engine-wizard /usr/share/nginx/html

CMD ["/start.sh"]
