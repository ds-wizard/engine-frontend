FROM nginx:1.27.4-alpine

RUN apk --purge del curl
RUN apk upgrade --no-cache

COPY app-registry/docker/nginx/default.conf /etc/nginx/conf.d/default.conf
COPY app-registry/docker/nginx/start.sh /start.sh

COPY dist/app-registry /usr/share/nginx/html

RUN chown -R nginx:nginx /usr/share/nginx/html && chmod -R 755 /usr/share/nginx/html && \
        chown -R nginx:nginx /var/cache/nginx && \
        chown -R nginx:nginx /var/log/nginx && \
        chown -R nginx:nginx /etc/nginx/conf.d
RUN touch /var/run/nginx.pid && \
        chown -R nginx:nginx /var/run/nginx.pid

USER nginx
EXPOSE 8080

CMD ["/start.sh"]
