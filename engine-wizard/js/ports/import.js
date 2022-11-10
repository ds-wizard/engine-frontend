var po2json = require('po2json')

module.exports = function (app) {
    app.ports.fileSelected.subscribe(fileSelected)
    app.ports.localeFileSelected.subscribe(localeFileSelected)
    app.ports.createDropzone.subscribe(createDropzone)
    app.ports.createLocaleDropzone.subscribe(createLocaleDropzone)

    function fileSelected(id) {
        var node = document.getElementById(id)
        if (node === null) {
            return
        }

        sendFile(node.files[0])
    }

    function localeFileSelected(id) {
        var node = document.getElementById(id)
        if (node === null) {
            return
        }

        convertLocaleFile(node.files[0], sendFile)
    }

    function createDropzone(id) {
        var node = document.getElementById(id)
        if (node === null) {
            return
        }

        node.ondrop = function (event) {
            event.preventDefault()
            event.stopPropagation()
            var file = getDroppedFile(event)
            sendFile(file)
        }
    }

    function createLocaleDropzone(id) {
        var node = document.getElementById(id)
        if (node === null) {
            return
        }

        node.ondrop = function (event) {
            event.preventDefault()
            event.stopPropagation()
            var file = getDroppedFile(event)
            convertLocaleFile(file, sendFile)
        }
    }

    function getDroppedFile(event) {
        if (event.dataTransfer.items) {
            for (var i = 0; i < event.dataTransfer.items.length; i++) {
                if (event.dataTransfer.items[i].kind === 'file') {
                    return event.dataTransfer.items[i].getAsFile()
                }
            }
        } else if (event.dataTransfer.files.length > 0) {
            return event.dataTransfer.files[0]
        }

        return null
    }

    function sendFile(file) {
        app.ports.fileContentRead.send(file)
    }

    function convertLocaleFile(file, cb) {
        var reader = new FileReader()
        reader.readAsText(file, "UTF-8")
        reader.onload = function (evt) {
            const parsed = po2json.parse(evt.target.result, {format: 'jed'})
            file = new File([JSON.stringify(parsed)], file.name)
            cb(file)
        }
    }
}
