module.exports = function (app) {
    app.ports.downloadFile.subscribe(downloadFile)

    function downloadFile(fileUrl) {
        const iframe = document.createElement('iframe')
        iframe.style.display = 'none'
        document.body.appendChild(iframe)
        iframe.src = fileUrl
    }
}
