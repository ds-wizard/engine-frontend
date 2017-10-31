FROM nginx:alpine
COPY dist /usr/share/nginx/html
COPY nginx/default.conf /etc/nginx/conf.d/default.conf

COPY nginx/start.sh /start.sh

CMD ["/start.sh"]
