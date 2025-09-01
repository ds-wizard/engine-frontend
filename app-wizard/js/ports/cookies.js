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

function getCookieConsent() {
    return !!localStorage.cookieConsent
}

function getGaEnabled() {
    return !!getGaId()
}

function getGaId() {
    if (window.wizard && window.wizard['gaID']) return window.wizard['gaID']
    return null
}

function init() {
    if (!getGaId() || !getCookieConsent()) return
    initGA(getGaId())
}

function registerCookiePorts(app) {
    app.ports.acceptCookies.subscribe(acceptCookies)

    function acceptCookies() {
        localStorage.cookieConsent = 1
        initGA(getGaId())
    }
}


module.exports = {
    getCookieConsent: getCookieConsent,
    getGaEnabled: getGaEnabled,
    init: init,
    registerCookiePorts: registerCookiePorts
}