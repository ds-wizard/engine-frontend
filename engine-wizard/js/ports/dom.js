const {waitForElement} = require('../utils.js');

module.exports = function (app) {
    app.ports.focus.subscribe(focus)
    app.ports.scrollIntoView.subscribe(scrollIntoView)
    app.ports.scrollIntoViewInstant.subscribe(scrollIntoViewInstant)
    app.ports.scrollIntoViewCenter.subscribe(scrollIntoViewCenter)
    app.ports.scrollToTop.subscribe(scrollToTop)
    app.ports.setScrollTopPort.subscribe(setScrollTopPort)
    app.ports.subscribeScrollTop.subscribe(subscribeScrollTop)

    function focus(elementSelector) {
        waitForElement(elementSelector, function ($element) {
            $element.focus()
        })
    }

    function scrollIntoView(elementSelector) {
        waitForElement(elementSelector, function ($element) {
            $element.scrollIntoView({
                behavior: 'smooth',
                block: 'start'
            })
        })
    }

    function scrollIntoViewInstant(elementSelector) {
        waitForElement(elementSelector, function ($element) {
            $element.scrollIntoView({
                behavior: 'instant',
                block: 'start'
            })
        })
    }

    function scrollIntoViewCenter(elementSelector) {
        waitForElement(elementSelector, function ($element) {
            $element.scrollIntoView({
                behavior: 'smooth',
                block: 'center'
            })
        })
    }

    function scrollToTop(elementSelector) {
        waitForElement(elementSelector, function ($element) {
            $element.scrollTop = 0
        })
    }

    function setScrollTopPort(data) {
        waitForElement(data.selector, function ($element) {
            $element.scrollTop = data.scrollTop
        })
    }

    function subscribeScrollTop(elementSelector) {
        waitForElement(elementSelector, function ($element) {
            $element.addEventListener('scroll', function () {
                app.ports.gotScrollTop.send({
                    selector: elementSelector,
                    scrollTop: $element.scrollTop
                })

            })
        })
    }
}
