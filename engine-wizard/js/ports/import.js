module.exports = function (app) {
    app.ports.fileSelected.subscribe(fileSelected);
    app.ports.createDropzone.subscribe(createDropzone);


    function fileSelected(id) {
        var node = document.getElementById(id);
        if (node === null) {
            return;
        }

        sendFile(node.files[0]);
    }


    function createDropzone(id) {
        var node = document.getElementById(id);
        if (node === null) {
            return;
        }

        node.ondrop = function (event) {
            event.preventDefault();
            event.stopPropagation();
            var file = getDroppedFile(event);
            sendFile(file);
        };
    }


    function getDroppedFile(event) {
        if (event.dataTransfer.items) {
            for (var i = 0; i < event.dataTransfer.items.length; i++) {
                if (event.dataTransfer.items[i].kind === 'file') {
                    return event.dataTransfer.items[i].getAsFile();
                }
            }
        } else if (event.dataTransfer.files.length > 0) {
            return event.dataTransfer.files[0];
        }

        return null;
    }


    function sendFile(file) {
        app.ports.fileContentRead.send(file)
    }
};
