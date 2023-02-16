FROM nginx:alpine

# Install alpine-pkg-glibc for dart-sass
RUN wget -q -O /etc/apk/keys/sgerrand.rsa.pub https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub
RUN wget https://github.com/sgerrand/alpine-pkg-glibc/releases/download/2.34-r0/glibc-2.34-r0.apk

# Fix for https://github.com/sgerrand/alpine-pkg-glibc/issues/185
RUN apk add --force-overwrite glibc-2.34-r0.apk
RUN apk fix --force-overwrite alpine-baselayout-data

# Install dart-sass
RUN wget https://github.com/sass/dart-sass/releases/download/1.58.0/dart-sass-1.58.0-linux-x64.tar.gz
RUN tar -xvzf ./dart-sass-1.58.0-linux-x64.tar.gz -C /opt/

# Dependencies needed to rebuild styles
COPY node_modules/bootstrap src/~bootstrap
COPY node_modules/@fortawesome src/~@fortawesome
COPY node_modules/flatpickr src/~flatpickr

COPY engine-wizard/docker/nginx/default.conf /etc/nginx/conf.d/default.conf
COPY engine-wizard/docker/profile.sh /configuration/profile.sh
COPY engine-wizard/docker/start.sh /start.sh

COPY engine-wizard/scss /src/scss
COPY dist/engine-wizard /usr/share/nginx/html

RUN chown -R nginx:nginx /src && chmod -R 755 /src && \
        chown -R nginx:nginx /usr/share/nginx/html && chmod -R 755 /usr/share/nginx/html && \
        chown -R nginx:nginx /var/cache/nginx && \
        chown -R nginx:nginx /var/log/nginx && \
        chown -R nginx:nginx /etc/nginx/conf.d
RUN touch /var/run/nginx.pid && \
        chown -R nginx:nginx /var/run/nginx.pid

USER nginx
EXPOSE 8080

CMD ["/start.sh"]
