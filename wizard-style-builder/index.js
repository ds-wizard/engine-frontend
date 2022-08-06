const compression = require('compression')
const express = require('express')
const helmet = require('helmet')
const morgan = require('morgan')
const core = require('./core')

const app = express()
const port = 3000


if (process.env.NODE_ENV === 'production') {
    app.use(helmet())
    app.use(morgan('combined'))
    app.use(compression())
}

app.use(express.json())

app.post('/simple', (req, res) => {
    let tempDir
    core.createTempDir()
        .then((folder) => tempDir = folder)
        .then(core.copySourceFiles)
        .then(core.createVariables(req.body))
        .then(core.renderSass)
        .then(core.postProcessCss(req.body))
        .then((result) => {
            res.setHeader('content-type', 'text/css')
            res.send(result)
        })
        .catch((err) => {
            res.status(500).send(err.toString())
        })
        .finally(() => {
            core.cleanTempDir(tempDir)
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
