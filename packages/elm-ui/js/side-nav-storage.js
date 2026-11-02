// Persistence for the navigation's width and collapsed state.
//
// Ui.SideNav itself has no port: an application already has somewhere to keep a
// preference — the wizard keeps its collapsed flag in the session — and a second
// storage mechanism underneath it would be one too many. This helper is only the
// shape the applications share: an app calls read() for its flags and register()
// to subscribe its own `uiStoreSideNav` port, and may ignore the collapsed flag
// when it already stores that elsewhere.
//
// The value is read before Elm starts and passed in as a flag, so the first
// paint is already the right width. Applying it afterwards would show the
// default sidebar for a frame and then move it.

const STORAGE_KEY = 'ui-side-nav'

function read() {
    try {
        const stored = JSON.parse(localStorage.getItem(STORAGE_KEY) || '{}')

        return {
            sideNavCollapsed: stored.collapsed === true,
            // null rather than a number of our own: Ui.SideNav owns the default
            // and the bounds, and a stored width outlives the release that
            // changes them.
            sideNavWidth: typeof stored.width === 'number' ? stored.width : null
        }
    } catch (e) {
        return { sideNavCollapsed: false, sideNavWidth: null }
    }
}

function register(app) {
    app.ports.uiStoreSideNav?.subscribe((state) => {
        try {
            localStorage.setItem(STORAGE_KEY, JSON.stringify(state))
        } catch (e) {
            // Private mode or blocked storage: the sidebar still works, it just
            // will not remember its width after a reload.
        }
    })
}

module.exports = { read, register }
