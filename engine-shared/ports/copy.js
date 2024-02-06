module.exports = function (app) {
    app.ports?.copyToClipboard?.subscribe(copyToClipboard)

    function copyToClipboard(string) {
        var input = document.createElement('textarea')
        input.style.cssText = 'position: absolute; left: -99999em';
        document.body.appendChild(input)
        input.value = string
        input.select()
        document.execCommand('copy')
        input.remove()
    }
}
