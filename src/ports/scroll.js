module.exports = function (app) {
    app.ports.scrollIntoView.subscribe(scrollIntoView)
    app.ports.scrollToTop.subscribe(scrollToTop)


    function scrollIntoView(elementId) {
        waitForElement(elementId, function ($element) {
            $element.scrollIntoView({
                behavior: 'smooth',
                block: 'start'
            })
        })
    }

    function scrollToTop(elementId) {
        waitForElement(elementId, function ($element) {
            $element.scrollTop = 0
        })
    }

    function waitForElement(elementId, callback, timeout) {
        timeout = timeout || 5000
        var step = 100
        var currentTime = 0
        var interval = setInterval(function () {
            var $element = document.getElementById(elementId)
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
