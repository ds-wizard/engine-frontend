// The clipboard port.
//
// navigator.clipboard is the API; the textarea is the fallback, which is what
// the existing port does unconditionally. It is still needed: the async API is
// unavailable on insecure origins and in a few older browsers, and a copy button
// that silently does nothing is worse than an execCommand call.

module.exports = (app) => {
    app.ports.uiCopyToClipboard?.subscribe(write)

    function write(value) {
        if (navigator.clipboard && navigator.clipboard.writeText) {
            navigator.clipboard.writeText(value).catch(() => legacyWrite(value))
            return
        }

        legacyWrite(value)
    }

    function legacyWrite(value) {
        const input = document.createElement('textarea')

        // Off-screen rather than hidden: an element that is not rendered cannot
        // be selected, and selection is what execCommand copies.
        input.style.cssText = 'position: absolute; left: -99999em'
        document.body.appendChild(input)
        input.value = value
        input.select()
        document.execCommand('copy')
        input.remove()
    }
}
