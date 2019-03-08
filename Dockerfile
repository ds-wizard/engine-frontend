FROM nginx:alpine

# Install sass in case we need to rebuild styles
RUN apk add --no-cache libsass sassc && rm -f /tmp/* /etc/apk/cache/*

# Dependencies needed to rebuild styles
COPY node_modules/bootstrap src/~bootstrap
COPY node_modules/bootswatch src/~bootswatch
COPY node_modules/font-awesome src/~font-awesome

COPY nginx/default.conf /etc/nginx/conf.d/default.conf
COPY nginx/start.sh /start.sh

COPY src/scss /src/scss
COPY dist /usr/share/nginx/html

CMD ["/start.sh"]
