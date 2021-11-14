FROM node:14-alpine

WORKDIR /usr/src/app

COPY package*.json ./
RUN npm ci --only=production

COPY src ./src
COPY index.js .

ENV NODE_ENV production
EXPOSE 3000
CMD ["node", "index.js"]
