module.exports = function (app) {
    app.ports.fileSelected.subscribe(fileSelected);
    app.ports.createDropzone.subscribe(createDropzone);


    function fileSelected(id) {
        var node = document.getElementById(id);
        if (node === null) {
            return;
        }

        readFile(node.files[0]);
    }


    function createDropzone(id) {
        var node = document.getElementById(id);
        if (node === null) {
            return;
        }

        node.addEventListener("drop", function (event) {
            event.preventDefault();
            event.stopPropagation();
            var file = getDroppedFile(event);
            readFile(file);
        });
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


    function readFile(file) {
        var reader = new FileReader();

        reader.onload = function (event) {
            var portData = {
                contents: event.target.result,
                filename: file.name
            };
            app.ports.fileContentRead.send(portData)
        };

        reader.readAsText(file);
    }
};
