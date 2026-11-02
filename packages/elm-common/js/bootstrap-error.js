// The screens shown before the Elm application starts: the bootstrap
// configuration could not be loaded, the tenant is still being seeded, or the
// data is being upgraded.
//
// The markup is `Ui.Page.Message`'s, so the screen matches the error pages the
// application draws itself. The stylesheet is linked in the head by
// webpack, so the classes are styled by the time any of this is shown.
//
// Accessibility, as in the application:
//   - the message is the page's `main`, with its one `h1`;
//   - the two screens that retry on their own are a `role="status"` region, and
//     their spinner is decorative — the heading says what is happening;
//   - the illustration is decorative (`alt=""`, and its wrapper `aria-hidden`),
//     because the heading and the sentence already say everything it does.

module.exports = init

function init() {

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
            message: 'Server responded with an error code ' + Number(errorCode) + '.<br>Please, contact the application provider.'
        })
    }

    function notSeededHTML() {
        return messageHTML({
            title: 'Preparing your application...',
            message: 'We\'re setting things up and will be done shortly.',
            retrying: true
        })
    }

    function housekeepingHTML() {
        return messageHTML({
            title: 'Housekeeping in progress',
            message: 'We are currently upgrading the data to the latest version to enhance your experience. This process will be completed shortly.',
            retrying: true
        })
    }

    function messageHTML({title, message, retrying = false}) {
        const spinner = retrying
            ? '<i class="ui-icon fas fa-circle-notch ui-spinner" aria-hidden="true"></i> '
            : ''
        const status = retrying ? ' role="status"' : ''

        return `<main>`
            + `<section class="ui-page-message">`
            + `<div class="ui-page-message__illustration">`
            // The same box Ui.Illustration draws, and the same file behind it:
            // this screen is shown before Elm starts, so it writes the markup
            // itself rather than asking for it.
            + `<span class="ui-illustration ui-illustration--error" aria-hidden="true"></span>`
            + `</div>`
            + `<div class="ui-stack"${status}>`
            + `<h1 class="ui-display">${spinner}${title}</h1>`
            + `<p class="ui-body">${message}</p>`
            + `</div>`
            + `</section>`
            + `</main>`
    }

    return {
        bootstrapErrorHTML,
        housekeepingHTML,
        notSeededHTML
    }
}
