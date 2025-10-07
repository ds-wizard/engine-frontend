module.exports = {
    getAdminApiUrl,
    getClientUrl,
    getDefaultApiUrl,
    getGuideLinks,
    getMaxUploadFileSize,
    getWebSocketThrottleDelay,
    isAdminEnabled
}

function getAdminApiUrl() {
    return getConfigProp('adminApiUrl', null)
}

function getClientUrl() {
    return getConfigProp('clientUrl', window.location.origin + '/wizard')
}

function getDefaultApiUrl() {
    return getConfigProp('apiUrl', window.location.origin + '/wizard-api')
}

function getGuideLinks() {
    return getConfigProp('guideLinks', {})
}

function getMaxUploadFileSize() {
    return getConfigProp('maxUploadFileSize')
}

function getWebSocketThrottleDelay() {
    return getConfigProp('webSocketThrottleDelay')
}

function isAdminEnabled() {
    return !!getConfigProp('admin')
}

function getConfigProp(prop, defaultValue) {
    return (window.app && window.app[prop]) || defaultValue
}
