FROM node:18-alpine

WORKDIR /usr/src/app

COPY package*.json ./
RUN npm ci --omit=dev

COPY src ./src
COPY core.js .
COPY index.js .

ENV NODE_ENV production
EXPOSE 3000
CMD ["node", "index.js"]
