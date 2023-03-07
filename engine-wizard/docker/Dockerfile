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
