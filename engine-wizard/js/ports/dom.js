module.exports = function (app) {
    app.ports.focus.subscribe(focus)
    app.ports.scrollIntoView.subscribe(scrollIntoView)
    app.ports.scrollToTop.subscribe(scrollToTop)

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

    function scrollToTop(elementSelector) {
        waitForElement(elementSelector, function ($element) {
            $element.scrollTop = 0
        })
    }

    function waitForElement(elementSelector, callback, timeout) {
        var $element = document.querySelector(elementSelector)
        if ($element instanceof HTMLElement) {
            callback($element)
        } else {
            timeout = timeout || 5000
            var step = 100
            var currentTime = 0
            var interval = setInterval(function () {
                var $element = document.querySelector(elementSelector)
                if ($element instanceof HTMLElement) {
                    clearInterval(interval)
                    callback($element)
                }

                currentTime += step
                if (currentTime >= timeout) {
                    clearInterval(interval)
                }
            }, step)
        }
    }
}
