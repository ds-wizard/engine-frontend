module.exports = {waitForElement}

function waitForElement(elementSelector, waitForVisible, callback, timeout) {
    var $element = document.querySelector(elementSelector)
    if ($element instanceof HTMLElement) {
        if (waitForVisible && $element.offsetParent) {
            callback($element)
            return
        } else if (!waitForVisible) {
            callback($element)
            return
        }
    }

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
