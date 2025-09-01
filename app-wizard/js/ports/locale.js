const po2json = require('po2json')

module.exports = function (app) {
    app.ports.convertLocaleFile?.subscribe(convertLocaleFile)


    function convertLocaleFile({fileName, fileContent}) {
        try {
            const parsed = po2json.parse(fileContent, {format: 'jed'})
            file = new File([JSON.stringify(parsed)], fileName)
            app.ports.localeConverted?.send(file)
        } catch {
        }
    }
}
