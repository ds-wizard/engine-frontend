FROM nginx:alpine

# Install sass in case we need to rebuild styles
RUN apk add --no-cache libsass sassc && rm -f /tmp/* /etc/apk/cache/*

# Dependencies needed to rebuild styles
COPY node_modules/bootstrap src/~bootstrap
COPY node_modules/@fortawesome src/~@fortawesome

COPY engine-wizard-research/docker/nginx/default.conf /etc/nginx/conf.d/default.conf
COPY engine-wizard-research/docker/profile.sh /configuration/profile.sh
COPY engine-wizard-research/docker/start.sh /start.sh

COPY engine-wizard-research/scss /src/scss
COPY dist/engine-wizard-research /usr/share/nginx/html

CMD ["/start.sh"]
