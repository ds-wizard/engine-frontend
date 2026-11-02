// Theme ports for the design system.
//
// Two jobs, and they are separate on purpose:
//
//   uiSetTheme  injects the derived brand roles as a <style> element, because
//               Ui.Theme.toStyleString emits *selector blocks* for both modes.
//               A style attribute cannot hold those: it silently drops
//               everything with a brace.
//               It then dispatches ui-theme-change on the document, which is
//               how a chart knows to repaint in the new brand.
//
//   uiSetMode   stamps data-theme on the document element, because every role
//               in _roles.scss is scoped to :root. An attribute anywhere below
//               the root matches nothing.
//
// The mode is persisted under the same key the inline script in index.ejs reads
// before first paint, so a stored choice never flashes.

const STYLE_ID = 'ui-theme'
const STORAGE_KEY = 'ui-mode'

module.exports = (app) => {
    app.ports.uiSetThemePort?.subscribe(setTheme)
    app.ports.uiSetModePort?.subscribe(setMode)

    function setTheme(css) {
        let style = document.getElementById(STYLE_ID)

        if (!style) {
            style = document.createElement('style')
            style.id = STYLE_ID
            document.head.appendChild(style)
        }

        style.textContent = css

        // Whatever paints from the roles outside CSS — a chart's canvas — has
        // to hear that they changed; nothing in the DOM it could observe does.
        document.dispatchEvent(new CustomEvent('ui-theme-change'))
    }

    function setMode(mode) {
        if (mode === 'light' || mode === 'dark') {
            document.documentElement.setAttribute('data-theme', mode)
        } else {
            // "system": stamp nothing and let prefers-color-scheme decide.
            document.documentElement.removeAttribute('data-theme')
        }

        try {
            if (mode === 'system') {
                localStorage.removeItem(STORAGE_KEY)
            } else {
                localStorage.setItem(STORAGE_KEY, mode)
            }
        } catch (e) {
            // Private mode or blocked storage: the choice still applies for
            // this page, it just will not survive a reload.
        }
    }
}
