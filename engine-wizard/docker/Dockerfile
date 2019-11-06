FROM nginx:alpine

# Install sass in case we need to rebuild styles
RUN apk add --no-cache libsass sassc && rm -f /tmp/* /etc/apk/cache/*

# Dependencies needed to rebuild styles
COPY node_modules/bootstrap src/~bootstrap
COPY node_modules/@fortawesome src/~@fortawesome

COPY engine-wizard/docker/nginx/default.conf /etc/nginx/conf.d/default.conf
COPY engine-wizard/docker/profile.sh /configuration/profile.sh
COPY engine-wizard/docker/start.sh /start.sh

COPY engine-wizard/scss /src/scss
COPY dist/engine-wizard /usr/share/nginx/html

CMD ["/start.sh"]
