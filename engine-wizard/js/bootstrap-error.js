module.exports = {
    bootstrapErrorHTML,
    notSeededHTML,
    housekeepingHTML
}

function bootstrapErrorHTML(errorCode) {
    if (!errorCode) {
        return messageHTML({
            title: 'Bootstrap Error',
            message: 'Configuration cannot be loaded due to server unavailable.<br>Please, contact the application provider.'
        })
    }
    if (errorCode === 404) {
        return messageHTML({
            title: 'Application Not Found or Inactive',
            message: 'We couldn\'t find an active application for this subdomain.<br>Please verify the details or contact support for assistance.'
        })
    }

    return messageHTML({
        title: 'Bootstrap Error',
        message: 'Server responded with an error code ' + errorCode + '.<br>Please, contact the application provider.'
    })
}

function housekeepingHTML() {
    return messageHTML({
        title: '<i class="fa fas fa-spinner fa-spin me-2 text-lighter"></i>Housekeeping in progress',
        message: 'We are currently upgrading the data to the latest version to enhance your experience. This process will be completed shortly.'
    })
}

function notSeededHTML() {
    return messageHTML({
        title: '<i class="fa fas fa-spinner fa-spin me-2 text-lighter"></i>Preparing your application...',
        message: 'We\'re setting things up and will be done shortly.'
    })
}

function messageHTML(title, message) {
    return '<div class="full-page-illustrated-message"><img src="/wizard/img/illustrations/undraw_bug_fixing.svg"><div><h1>' + title + '</h1><p>' + message + '</p></div></div>'
}