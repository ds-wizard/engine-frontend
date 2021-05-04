function initGA(id) {
    if (!id) return

    var script = document.createElement('script')
    script.src = "https://www.googletagmanager.com/gtag/js?id=" + id
    script.async = true
    document.head.append(script)

    window.dataLayer = window.dataLayer || []
    window.gtag = function () {
        dataLayer.push(arguments)
    }
    window.gtag('js', new Date())
    window.gtag('config', id)

    history.pushState = function () {
        History.prototype.pushState.apply(history, arguments)
        window.gtag('config', id)
    }
}

module.exports = initGA
