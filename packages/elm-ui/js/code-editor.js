// A code field, on CodeMirror 6.
//
// The two existing ones (internal and wizard) differ only in which languages
// they load. What is different here is in three places, and all three are the
// same kind of problem the other wrapped widgets had:
//
//   Tab          The existing editors bind Tab to indent. That is a keyboard
//                trap: once focus is in the editor there is no way out of it
//                with the keyboard, which fails 2.1.2 — CodeMirror's own
//                documentation says as much about indentWithTab. Tab moves
//                focus here, and indenting is Mod-] and Mod-[, which
//                CodeMirror's default keymap already provides.
//
//   Focus        Their stylesheet carries `.cm-focused { outline: none }`,
//                which removes the only indication of where the keyboard is
//                (2.4.7). The editor is given the system's focus ring instead.
//
//   The name     A CodeMirror editor is a contenteditable with role="textbox"
//                and no name at all. The field's label is pointed at with
//                aria-labelledby, because `for` cannot reach it.
//
// Colors come from roles rather than from a CodeMirror theme: the highlight
// style names classes instead of hex, so the stylesheet decides what a keyword
// looks like and dark mode needs no second theme.

const { Compartment, EditorState } = require('@codemirror/state')
const { EditorView } = require('@codemirror/view')
const { basicSetup } = require('codemirror')
const { HighlightStyle, StreamLanguage, syntaxHighlighting } = require('@codemirror/language')
const { tags } = require('@lezer/highlight')

const { python } = require('@codemirror/lang-python')
const { css } = require('@codemirror/lang-css')
const { html } = require('@codemirror/lang-html')
const { xml } = require('@codemirror/lang-xml')
const { jinja2 } = require('@codemirror/legacy-modes/mode/jinja2')
// JSON is JavaScript's object syntax, and the JavaScript mode is the one that
// ships with the editor — there is no separate JSON package installed, and one
// is not worth adding to highlight a response body.
const { javascript } = require('@codemirror/lang-javascript')

// Every language both existing editors know between them. One list, because a
// component that supports a different set per application is two components.
const LANGUAGES = {
    python: () => python(),
    css: () => css(),
    html: () => html(),
    xml: () => xml(),
    jinja2: () => StreamLanguage.define(jinja2),
    json: () => javascript()
}

// The content types the document-template editor maps from. Kept here so the
// mapping lives with the languages it maps to.
const CONTENT_TYPES = {
    'application/xml': 'xml',
    'text/css': 'css',
    'text/html': 'html',
    'text/html+jinja2': 'jinja2',
    'text/x-python': 'python'
}

function languageFor(name) {
    const language = LANGUAGES[name]
    return language ? language() : null
}

function languageForContentType(contentType) {
    return CONTENT_TYPES[contentType] || ''
}

// Classes, not colors. What a keyword looks like is a decision for the
// stylesheet, where the roles are and where dark mode already happens.
const highlightStyle = HighlightStyle.define([
    { tag: [tags.comment, tags.lineComment, tags.blockComment, tags.docComment], class: 'ui-syntax-comment' },
    { tag: [tags.keyword, tags.controlKeyword, tags.moduleKeyword, tags.operatorKeyword, tags.self, tags.null, tags.atom], class: 'ui-syntax-keyword' },
    { tag: [tags.string, tags.special(tags.string), tags.regexp], class: 'ui-syntax-string' },
    { tag: [tags.number, tags.bool, tags.literal], class: 'ui-syntax-number' },
    { tag: [tags.function(tags.variableName), tags.definition(tags.variableName), tags.tagName, tags.propertyName, tags.attributeName, tags.className, tags.typeName], class: 'ui-syntax-name' }
])

function define() {
    class CodeEditorElement extends HTMLElement {
        constructor() {
            super()
            this._value = ''
            this._language = ''
            this._view = null

            // Line wrapping is the one thing that changes while the editor is
            // open, so it lives in a compartment: reconfiguring it keeps the
            // document, the cursor and the scroll position, where rebuilding
            // the state would put all three back to the top.
            this._wrapping = new Compartment()
        }

        static get observedAttributes() {
            return ['language', 'input-id', 'label', 'labelledby', 'describedby', 'invalid', 'readonly', 'disabled', 'wrap']
        }

        attributeChangedCallback(name, oldValue, newValue) {
            if (oldValue === newValue || !this._view) return

            if (name === 'wrap') {
                this._view.dispatch({ effects: this._wrapping.reconfigure(this._wrapExtension()) })
                return
            }

            this._rebuild()
        }

        get editorValue() {
            return this._value
        }

        set editorValue(value) {
            const next = value == null ? '' : String(value)
            if (next === this._value) return

            this._value = next
            if (this._view) this._rebuild()
        }

        connectedCallback() {
            if (this._view) return

            this._view = new EditorView({ state: this._state(), parent: this })
        }

        disconnectedCallback() {
            if (this._view) {
                this._view.destroy()
                this._view = null
            }
        }

        // CodeMirror keeps its own document, so a change from outside replaces
        // the state rather than patching it. Only when the text actually
        // differs: replacing it on every echo of what was just typed would put
        // the cursor back at the start on every keystroke.
        _rebuild() {
            this._view.setState(this._state())
        }

        _state() {
            const readOnly = this.hasAttribute('readonly') || this.hasAttribute('disabled')

            const extensions = [
                basicSetup,

                // No Tab binding. CodeMirror's default keymap indents with
                // Mod-] and Mod-[, which leaves Tab doing what Tab does
                // everywhere else — the alternative is an editor a keyboard
                // user cannot leave (2.1.2).
                syntaxHighlighting(highlightStyle),

                // Long lines wrap unless the caller says otherwise: a field in a
                // form has no room to scroll sideways in. A file editor that
                // fills the screen does, and offers the choice.
                this._wrapping.of(this._wrapExtension()),
                EditorView.updateListener.of((update) => {
                    if (!update.docChanged) return

                    this._value = update.state.doc.toString()
                    this.dispatchEvent(new CustomEvent('editorChanged', { bubbles: true }))
                }),
                EditorView.domEventHandlers({
                    focus: () => this.dispatchEvent(new Event('focus', { bubbles: true })),
                    blur: () => this.dispatchEvent(new Event('blur', { bubbles: true }))
                }),
                EditorView.contentAttributes.of(this._contentAttributes()),

                // Read-only rather than not editable: the text can still be
                // reached, selected and copied with the keyboard, which is the
                // difference between showing code and showing a picture of it.
                EditorState.readOnly.of(readOnly)
            ]

            const language = languageFor(this.getAttribute('language'))
            if (language) extensions.push(language)

            return EditorState.create({ doc: this._value, extensions })
        }

        _wrapExtension() {
            return this.getAttribute('wrap') === 'off' ? [] : EditorView.lineWrapping
        }

        _contentAttributes() {
            const attributes = {}

            const id = this.getAttribute('input-id')
            if (id) attributes.id = id

            const labelledby = this.getAttribute('labelledby')
            if (labelledby) attributes['aria-labelledby'] = labelledby

            // A read-only block has no field label to point at, so it is named
            // directly. Either way the name lands on the content, which is the
            // element carrying the role.
            const label = this.getAttribute('label')
            if (label && !labelledby) attributes['aria-label'] = label

            const describedby = this.getAttribute('describedby')
            if (describedby) attributes['aria-describedby'] = describedby

            if (this.hasAttribute('invalid')) attributes['aria-invalid'] = 'true'

            return attributes
        }
    }

    customElements.define('ui-code-editor', CodeEditorElement)
}

if (typeof customElements !== 'undefined' && typeof HTMLElement !== 'undefined') {
    define()
}

module.exports = { LANGUAGES, CONTENT_TYPES, languageForContentType, highlightStyle }
