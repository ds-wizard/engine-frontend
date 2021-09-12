# Contributing

When contributing to this repository, please first discuss the change you wish to make via issue, email, or any other
method with the owners of this repository before making a change.

## Development and Code Style

- Use [elm-format](https://elmprogramming.com/installation.html#installing-elm-format)
- Use [elm-analyse](https://stil4m.github.io/elm-analyse/)
- All changes must pass CI (both build and code style checks)

## Pull Request Process

1. Ensure any unnecessary install or build dependencies and other generated files are removed (adjust `.gitignore` or `.dockerignore` if necessary).
2. Explain the changes and update in the Pull Request message. If it affects our [User Guide](https://guide.ds-wizard.org), 
   state explicitly how it should be changed.
3. Propose changes to the [E2E test suite](https://github.com/ds-wizard/dsw-e2e-tests) if applicable (as PR or at least describe what changes are needed).
4. Be ready to communicate about the Pull Request and make changes if required by reviewers.
5. The Pull Request may be merged once it passes the review and automatic checks.

## Gitflow Workflow

We use the standard [Gitflow Workflow](https://www.atlassian.com/git/tutorials/comparing-workflows/gitflow-workflow):

* __master__ branch is used only for releases (and eventually hotfixes), this branch is also protected on GitHub (pull
  requests with review and all checks must pass)
* __develop__ branch is used for development and as a base for following development branches of features, support
  stuff, and as a base for releases
* __feature/*__ (base develop, rebase-merged back to develop when done)
* __chore/*__ (like the feature but semantically different, not the feature but some chore, e.g., cleanup or update of
  Dockerfile)
* __fix/*__ (like the feature but semantically different, not something new but fix of a non-critical bug)
* __release/*__ (base develop, merged to master and develop when ready for release+tag)
* __hotfix/*__ (base master, merged to master and develop)

Please note, that for tasks from [our Jira](https://ds-wizard.atlassian.net/projects/DSW/issues), we use such
as `[DSW-XXX]` identifying the project and task number.

## Release Management

For the release management we use (aside from the [Gitflow Workflow](https://www.atlassian.com/git/tutorials/comparing-workflows/gitflow-workflow)):

* [Semantic versioning](https://semver.org)
* Release Candidates - X.Y.Z-rc.N should be created if don’t expect any problems (in that case use alpha or beta), and
  make a walkthrough to verify its functionality according to the manuals finally - it also verifies that the
  documentation is up to date with the new version.
* Docker Hub image - in case of release, Docker image with the same tag will be created automatically.
* Compatibility in DSW - the matching major and minor version of DSW components must be compatible.

The changes must be captured in our [User Guide](https://guide.ds-wizard.org).
