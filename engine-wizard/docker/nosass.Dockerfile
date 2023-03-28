FROM nginx:alpine

RUN apk --purge del curl


COPY engine-wizard/docker/nginx/default.conf /etc/nginx/conf.d/default.conf
COPY engine-wizard/docker/profile.sh /configuration/profile.sh
COPY engine-wizard/docker/start.sh /start.sh

COPY dist/engine-wizard /usr/share/nginx/html

RUN chown -R nginx:nginx /usr/share/nginx/html && chmod -R 755 /usr/share/nginx/html && \
        chown -R nginx:nginx /var/cache/nginx && \
        chown -R nginx:nginx /var/log/nginx && \
        chown -R nginx:nginx /etc/nginx/conf.d
RUN touch /var/run/nginx.pid && \
        chown -R nginx:nginx /var/run/nginx.pid

USER nginx
EXPOSE 8080

CMD ["/start.sh"]
