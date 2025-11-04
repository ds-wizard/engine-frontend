module.exports = init

function init(moduleName) {

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


    function notSeededHTML() {
        return messageHTML({
            title: '<i class="fa fas fa-spinner fa-spin me-2 text-lighter"></i>Preparing your application...',
            message: 'We\'re setting things up and will be done shortly.'
        })
    }

    function housekeepingHTML() {
        return messageHTML({
            title: '<i class="fa fas fa-spinner fa-spin me-2 text-lighter"></i>Housekeeping in progress',
            message: 'We are currently upgrading the data to the latest version to enhance your experience. This process will be completed shortly.'
        })
    }


    function messageHTML(error) {
        return `<div class="container mt-5"><div class="row justify-content-center"><div class="col-4"><img class="w-100" src="/${moduleName}/assets/illustrations/undraw_bug_fixing.svg"></div><div class="col-6 d-flex flex-column justify-content-center"><h1>${error.title}</h1><p class="font-lg">${error.message}</p></div></div></div>`
    }

    return {
        bootstrapErrorHTML,
        housekeepingHTML,
        notSeededHTML
    }
}
