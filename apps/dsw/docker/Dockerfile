FROM nginx:1.30-alpine

RUN apk --purge del curl
RUN apk upgrade --no-cache

COPY apps/dsw/docker/nginx/default.conf /etc/nginx/conf.d/default.conf
COPY apps/dsw/docker/start.sh /start.sh

COPY dist/dsw /usr/share/nginx/html

# The page as built, for start.sh to add the head extras to
RUN mkdir /template && cp /usr/share/nginx/html/index.html /template/index.html

RUN chown -R nginx:nginx /usr/share/nginx/html && chmod -R 755 /usr/share/nginx/html && \
        chown -R nginx:nginx /var/cache/nginx && \
        chown -R nginx:nginx /var/log/nginx && \
        chown -R nginx:nginx /etc/nginx/conf.d
RUN touch /var/run/nginx.pid && \
        chown -R nginx:nginx /var/run/nginx.pid

USER nginx
EXPOSE 8080

CMD ["/start.sh"]
