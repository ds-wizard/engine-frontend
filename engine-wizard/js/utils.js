module.exports = {waitForElement}

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
