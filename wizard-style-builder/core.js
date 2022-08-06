const fs = require('fs')
const fse = require('fs-extra')
const os = require('os')
const path = require('path')
const sass = require('sass')

const replaceAll = (string, searchValue, replaceValue) => string.split(searchValue).join(replaceValue)

const createTempDir = () => fs.promises.mkdtemp(path.join(os.tmpdir(), 'src-'))

const copySourceFiles = (dst) => new Promise((resolve, reject) => {
    fse.copy('src', dst, (err) => {
        if (err) {
            reject(err)
        } else {
            resolve(dst)
        }
    })
})

const createVariables = (options) => (folder) => new Promise((resolve, reject) => {
    let variables = ''

    if (options.logoUrl) {
        variables += `$side-navigation-logo-url: url(${options.logoUrl});\n`
    }

    if (options.primaryColor) {
        variables += `$primary: ${options.primaryColor};\n`
    }

    if (options.illustrationsColor) {
        variables += `$illustrations-color: ${options.illustrationsColor};\n`
    }

    const variablesFile = `${folder}/scss/customizations/_variables.scss`

    fs.writeFile(variablesFile, variables, (err) => {
        if (err) {
            reject(err)
        } else {
            resolve(folder)
        }
    })
})

const renderSass = (folder) => new Promise((resolve, reject) => {
    const mainPath = `${folder}/scss/main.scss`

    sass.compileAsync(mainPath, {
        loadPaths: [folder],
        style: 'compressed',
        quietDeps: true
    })
        .then((result) => {
            const stripComment = (string) => string.replace(/\/\*[^*]*\*+([^\/][^*]*\*+)*\//, '')
            const css = stripComment(stripComment(result.css.toString()))
            resolve(css)
        })
        .catch((err) => {
            reject(err)

        })
})

const postProcessCss = (options) => (css) => new Promise((resolve) => {
    if (options.clientUrl) {
        const clientUrl = options.clientUrl.replace(/\/$/, '')
        const cssWithFonts = replaceAll(css, 'url("~@fortawesome/fontawesome-free/webfonts/', `url("${clientUrl}/`)
        const cssWithLogoUrl = replaceAll(cssWithFonts, 'url(../img/logo.svg', `url(${clientUrl}/img/logo.svg`)
        const cssWithLsLoginUrl = replaceAll(cssWithLogoUrl, 'url(../img/ls-login.png)', `url(${clientUrl}/img/ls-login.png)`)
        resolve(cssWithLsLoginUrl)
    } else {
        resolve(css)
    }
})

const cleanTempDir = (folder) => {
    fs.rm(folder, {recursive: true}, () => {})
}

module.exports = {
    createTempDir,
    copySourceFiles,
    createVariables,
    renderSass,
    postProcessCss,
    cleanTempDir,
}
