# Data Stewardship Wizard Engine Frontend

[![User Guide](https://img.shields.io/badge/docs-User%20Guide-informational)](https://guide.ds-wizard.org)
[![Engine Frontend CI](https://github.com/ds-wizard/engine-frontend/actions/workflows/pipeline.yml/badge.svg)](https://github.com/ds-wizard/engine-frontend/actions)
[![License](https://img.shields.io/github/license/ds-wizard/engine-frontend)](LICENSE)
[![CII Best Practices](https://bestpractices.coreinfrastructure.org/projects/4975/badge)](https://bestpractices.coreinfrastructure.org/projects/4975)

Web clients for Data Stewardship Wizard, and the design system they are built on. An
[Elm](https://elm-lang.org) 0.19.1 monorepo built with webpack.

## Layout

```
apps/
  dsw/                    the Data Stewardship Wizard client (with its Docker image in docker/)
  registry/               the registry client (with its Docker image in docker/)
packages/
  elm-ui/                 the design system
  elm-common/             composites and whole pages built over the design system
  elm-vendor/             generic Elm modules
  elm-kernel-replacements/  patched Elm kernel packages
test/elm/                 the elm-test suites
tools/                    build, translations (i18n), repository checks, guide links
```

## Commands

```bash
npm ci
npm run replace-kernel-packages   # once after npm ci

npm run start:dsw                 # dev server, http://localhost:8080
npm run start:registry            # dev server, http://localhost:8081
npm run build                     # dist/dsw and dist/registry
npm run build:dsw / build:registry
npm run pot                       # translations template, locale/dsw.pot
npm test                          # elm-test, repository checks and ui tests
npm run test:elm / test:checks / test:ui
```

## Running a client against an API

Each client reads its configuration from `config.js`. For development, create
`apps/<app>/static/config.js` (it is git-ignored):

```js
window.app = {
    apiUrl: 'http://localhost:3000'
}
```

Then run `npm run start:dsw` or `npm run start:registry`.

## Docker

```bash
npm run build:dsw
docker build -f apps/dsw/docker/Dockerfile -t wizard-client .
docker run -p 8080:8080 -e API_URL=https://api.example.org wizard-client

npm run build:registry
docker build -f apps/registry/docker/Dockerfile -t registry-client .
docker run -p 8080:8080 -e API_URL=https://registry-api.example.org registry-client
```

The dsw client is served at the root. `API_URL` is written to `config.js` when the container
starts. The registry image also accepts `APP_TITLE`. In the dsw image, a file mounted at
`/src/head-extra.html` is added to the page just before `</head>`.

## Contributing

For contributing guidelines, please read [CONTRIBUTING](CONTRIBUTING.md).

## License

This project is licensed under the Apache License v2.0 - see the
[LICENSE](LICENSE) file for more details.
