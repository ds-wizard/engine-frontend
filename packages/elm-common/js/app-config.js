module.exports = {
    getClientUrl,
    getDefaultApiUrl,
    getGuideLinks,
    getMaxUploadFileSize,
    getNewsUrl,
    getPrivacyPolicyUrl,
    getTermsOfServiceUrl,
    getUrlCheckerUrl,
    getWebSocketThrottleDelay
}

function getClientUrl() {
    return getConfigProp('clientUrl', window.location.origin)
}

function getDefaultApiUrl() {
    return getConfigProp('apiUrl', window.location.origin + '/api')
}

function getGuideLinks() {
    return getConfigProp('guideLinks', {})
}

function getMaxUploadFileSize() {
    return getConfigProp('maxUploadFileSize')
}

function getNewsUrl() {
    return getConfigProp('newsUrl', null)
}

// The deployment's own privacy policy and terms of service, for an instance
// whose configuration names neither.
function getPrivacyPolicyUrl() {
    return getConfigProp('privacyPolicy', null)
}

function getTermsOfServiceUrl() {
    return getConfigProp('termsOfService', null)
}

function getUrlCheckerUrl() {
    return getConfigProp('urlCheckerUrl', null)
}

function getWebSocketThrottleDelay() {
    return getConfigProp('webSocketThrottleDelay')
}

function getConfigProp(prop, defaultValue) {
    return (window.app && window.app[prop]) || defaultValue
}
