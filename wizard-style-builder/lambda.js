const core = require('./core')

exports.handler = async function (event) {
    const eventBody = JSON.parse(event.body)
    let tempDir
    return core.createTempDir()
        .then((folder) => tempDir = folder)
        .then(core.copySourceFiles)
        .then(core.createVariables(eventBody))
        .then(core.renderSass)
        .then(core.postProcessCss(eventBody))
        .then((result) => ({
            statusCode: 200,
            headers: {
                'Content-Type': 'text/css'
            },
            body: result
        }))
        .finally(() => {
            core.cleanTempDir(tempDir)
        })
}
