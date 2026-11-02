// Scrolling and focus for applications on the design system
// (Common.Ports.Dom explains what each one is for).
//
// Every command waits a frame before it looks: Elm renders on the next animation
// frame after an update, and a command runs before that render, so looking at
// once would find the page as it was — the old chapter, not the question just
// built.
const {waitForElement} = require('../utils.js')

const SETTLE_MS = 150

module.exports = function (app) {
    // Whether the reader's last action was a key rather than a pointer: focus
    // moved from script is ringed after a key press only.
    let lastInputWasKey = false
    window.addEventListener('keydown', () => { lastInputWasKey = true }, true)
    window.addEventListener('pointerdown', () => { lastInputWasKey = false }, true)

    app.ports.focus?.subscribe(focus)
    app.ports.focusAndSetCaretPort?.subscribe(focusAndSetCaretPort)
    app.ports.commonScrollToAndFocus?.subscribe(scrollToAndFocus)
    app.ports.commonWatchScrollTop?.subscribe(watchScrollTop)
    app.ports.commonSetScrollTop?.subscribe(setScrollTop)
    app.ports.commonFocusWithCaret?.subscribe(focusWithCaret)

    // Move focus, and nothing else. The one command that does not wait a frame:
    // it is asked for after the render that put its target on the page.
    function focus(elementSelector) {
        waitForElement(elementSelector, true, function ($element) {
            $element.focus()
        })
    }

    function focusAndSetCaretPort({elementSelector, caretPos}) {
        waitForElement(elementSelector, true, function ($element) {
            $element.focus()
            $element.setSelectionRange(caretPos, caretPos)
        })
    }

    function afterRender(callback) {
        window.requestAnimationFrame(() => window.requestAnimationFrame(callback))
    }

    // Focus a text box and put the caret where it was asked for — after the
    // render, so the value Elm is about to write does not take the caret to the
    // end with it.
    function focusWithCaret({selector, caret}) {
        afterRender(() => {
            waitForElement(selector, false, ($element) => {
                if (typeof $element.setSelectionRange !== 'function') return
                $element.focus()
                const at = Math.min(caret, $element.value.length)
                $element.setSelectionRange(at, at)
            })
        })
    }

    // After a key press the focus moved from script is ringed by hand
    // (`.ui-focus-forced`, `_reset.scss`): browsers differ in whether
    // `:focus-visible` follows focus that script moves. Taken away again by the
    // next thing the reader does.
    function forceFocusRing($element) {
        $element.classList.add('ui-focus-forced')

        const clear = function () {
            $element.classList.remove('ui-focus-forced')
            window.removeEventListener('pointerdown', clear, true)
            window.removeEventListener('keydown', clear, true)
            $element.removeEventListener('blur', clear)
        }

        window.addEventListener('pointerdown', clear, true)
        window.addEventListener('keydown', clear, true)
        $element.addEventListener('blur', clear)
    }

    // The nearest ancestor that scrolls — `overflow` auto or scroll, not hidden.
    function scrollContainer($element) {
        for (let $node = $element.parentElement; $node; $node = $node.parentElement) {
            const overflow = window.getComputedStyle($node).overflowY
            if (overflow === 'auto' || overflow === 'scroll') return $node
        }
        return null
    }

    // Not `scrollIntoView`: that scrolls every ancestor that can be scrolled,
    // and a box with `overflow: hidden` can be — so a workspace inside a page
    // that clips it would slide up under its own frame, the toolbar out of
    // sight and an empty band where it had been. Only the region the target
    // is in scrolls, to the target's top less its `scroll-margin`.
    function scrollTo($element, smooth) {
        const $container = scrollContainer($element)
        if (!$container) {
            $element.scrollIntoView({behavior: smooth ? 'smooth' : 'instant', block: 'start'})
            return
        }
        const margin = parseFloat(window.getComputedStyle($element).scrollMarginTop) || 0
        const top = $element.getBoundingClientRect().top - $container.getBoundingClientRect().top + $container.scrollTop - margin
        if (typeof $container.scrollTo === 'function') {
            $container.scrollTo({top: Math.max(0, top), behavior: smooth ? 'smooth' : 'instant'})
        } else {
            $container.scrollTop = Math.max(0, top)
        }
    }

    function scrollToAndFocus({selector, focus, smooth}) {
        afterRender(() => {
            waitForElement(selector, false, ($element) => {
                const reduceMotion = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches
                scrollTo($element, smooth && !reduceMotion)
                // A title or a heading is not a control: it takes focus from
                // script only, and never from Tab.
                const $focus = document.querySelector(focus)
                if ($focus) {
                    if (!$focus.hasAttribute('tabindex')) $focus.setAttribute('tabindex', '-1')
                    $focus.focus({preventScroll: true})
                    if (lastInputWasKey) forceFocusRing($focus)
                }
            })
        })
    }

    // One listener per element: the questionnaire asks again whenever it comes
    // back, and the element it finds may be the one it already watches.
    const watched = new WeakSet()

    function watchScrollTop(selector) {
        afterRender(() => {
            waitForElement(selector, false, ($element) => {
                if (watched.has($element)) return
                watched.add($element)
                let timer = null
                $element.addEventListener('scroll', () => {
                    clearTimeout(timer)
                    timer = setTimeout(() => {
                        app.ports.commonGotScrollTop?.send({selector, scrollTop: Math.round($element.scrollTop)})
                    }, SETTLE_MS)
                }, {passive: true})
            })
        })
    }

    function setScrollTop({selector, scrollTop}) {
        afterRender(() => {
            waitForElement(selector, false, ($element) => {
                $element.scrollTop = scrollTop
            })
        })
    }
}
