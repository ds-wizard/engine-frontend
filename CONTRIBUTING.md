# Contributing

When contributing to this repository, please first discuss the change you wish to make via issue, email, or any other
method with the owners of this repository before making a change.

## Development and Code Style

- Use [elm-format](https://github.com/avh4/elm-format) 0.8.8 (CI runs `elm-format --validate`)
- Run `npm test` before opening a pull request (Elm tests, repository checks, and UI tests)
- American English spelling in code, comments, and strings
- All changes must pass CI (both build and code style checks)

## Pull Request Process

1. Ensure any unnecessary install or build dependencies and other generated files are removed (adjust `.gitignore` or `.dockerignore` if necessary).
2. Explain the changes and update in the Pull Request message. If it affects our [User Guide](https://guide.ds-wizard.org), 
   state explicitly how it should be changed.
3. Propose changes to the [E2E test suite](https://github.com/ds-wizard/dsw-e2e-tests) if applicable (as PR or at least describe what changes are needed).
4. Be ready to communicate about the Pull Request and make changes if required by reviewers.
5. The Pull Request may be merged once it passes the review and automatic checks.

## Branching

We use a trunk-based workflow:

* __main__ is the main branch, protected on GitHub (pull requests with review and all checks must pass). Releases are
  tagged from it.
* __feature/*__, __chore/*__ and __fix/*__ branches are based on main and rebase-merged back to it when done
  (a chore is a cleanup or a maintenance task, e.g., an update of a Dockerfile; a fix is a fix of a non-critical bug)
* __hotfix/*__ branches are based on the release tag being fixed and merged to main

Please note, that for tasks from [our Jira](https://ds-wizard.atlassian.net/projects/DSW/issues), we use such
as `[DSW-XXX]` identifying the project and task number.

## Release Management

For the release management we use:

* [Semantic versioning](https://semver.org)
* Release Candidates - X.Y.Z-rc.N should be created if don’t expect any problems (in that case use alpha or beta), and
  make a walkthrough to verify its functionality according to the manuals finally - it also verifies that the
  documentation is up to date with the new version.
* Docker Hub image - in case of release, Docker image with the same tag will be created automatically.
* Compatibility in DSW - the matching major and minor version of DSW components must be compatible.

The changes must be captured in our [User Guide](https://guide.ds-wizard.org).
