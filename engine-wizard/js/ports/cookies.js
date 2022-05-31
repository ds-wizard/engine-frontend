var initGA = require('../../../engine-shared/js/ga')

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