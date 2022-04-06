const compression = require('compression')
const express = require('express')
const fs = require('fs')
const fse = require('fs-extra')
const helmet = require('helmet')
const morgan = require('morgan')
const os = require('os')
const path = require('path')
const sass = require('node-sass')

const app = express()
const port = 3000


if (process.env.NODE_ENV === 'production') {
    app.use(helmet())
    app.use(morgan('combined'))
    app.use(compression())
}

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

    sass.render({
        file: mainPath,
        includePaths: [folder],
        outputStyle: 'compressed'
    }, (err, result) => {
        if (err) {
            reject(err)
        } else {
            const stripComment = (string) => string.replace(/\/\*[^*]*\*+([^\/][^*]*\*+)*\//, '')
            const css = stripComment(stripComment(result.css.toString()))
            resolve(css)
        }
    })
})

const postProcessCss = (options) => (css) => new Promise((resolve) => {
    if (options.clientUrl) {
        const clientUrl = options.clientUrl.replace(/\/$/, '')
        const cssWithFonts = replaceAll(css, 'url("~@fortawesome/fontawesome-free/webfonts/', `url("${clientUrl}/`)
        const cssWitLogoUrl = replaceAll(cssWithFonts, 'url(../img/logo.svg', `url(${clientUrl}/img/logo.svg`)
        resolve(cssWitLogoUrl)
    } else {
        resolve(css)
    }
})

const cleanTempDir = (folder) => {
    fs.rmdir(folder, {recursive: true}, () => {})
}


app.use(express.json())

app.post('/simple', (req, res) => {
    let tempDir
    createTempDir()
        .then((folder) => tempDir = folder)
        .then(copySourceFiles)
        .then(createVariables(req.body))
        .then(renderSass)
        .then(postProcessCss(req.body))
        .then((result) => {
            res.setHeader('content-type', 'text/css')
            res.send(result)
        })
        .catch((err) => {
            res.status(500).send(err)
        })
        .finally(() => {
            cleanTempDir(tempDir)
        })
})

module.exports = app.listen(port, () => {
    console.log('________    ___________      __    _________ __          .__           __________      .__.__       .___            \n' +
        '\\______ \\  /   _____/  \\    /  \\  /   _____//  |_ ___.__.|  |   ____   \\______   \\__ __|__|  |    __| _/___________ \n' +
        ' |    |  \\ \\_____  \\\\   \\/\\/   /  \\_____  \\\\   __<   |  ||  | _/ __ \\   |    |  _/  |  \\  |  |   / __ |/ __ \\_  __ \\\n' +
        ' |    `   \\/        \\\\        /   /        \\|  |  \\___  ||  |_\\  ___/   |    |   \\  |  /  |  |__/ /_/ \\  ___/|  | \\/\n' +
        '/_______  /_______  / \\__/\\  /   /_______  /|__|  / ____||____/\\___  >  |______  /____/|__|____/\\____ |\\___  >__|   \n' +
        '        \\/        \\/       \\/            \\/       \\/               \\/          \\/                    \\/    \\/       ')
})
