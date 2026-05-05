module.exports = function (app) {
    app.ports.formScrollToInvalidField?.subscribe(formScrollToInvalidField)

    function formScrollToInvalidField() {
        window.requestAnimationFrame(function () {
            const $el = document.querySelector('.is-invalid')
            if ($el) {
                $el.scrollIntoView({
                    block: "center",
                    behavior: "smooth"
                })
            }
        })
    }
}