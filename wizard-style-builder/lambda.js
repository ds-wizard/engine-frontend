const core = require('./core')

exports.handler = async function (event) {
    let tempDir
    return core.createTempDir()
        .then((folder) => tempDir = folder)
        .then(core.copySourceFiles)
        .then(core.createVariables(event))
        .then(core.renderSass)
        .then(core.postProcessCss(event))
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
