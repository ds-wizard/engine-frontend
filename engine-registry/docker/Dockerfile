FROM nginx:alpine

COPY engine-registry/docker/nginx/default.conf /etc/nginx/conf.d/default.conf
COPY engine-registry/docker/nginx/start.sh /start.sh

COPY dist/engine-registry /usr/share/nginx/html

CMD ["/start.sh"]
