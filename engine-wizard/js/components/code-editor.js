const {EditorState} = require('@codemirror/state')
const {EditorView, keymap} = require('@codemirror/view')
const {indentWithTab} = require('@codemirror/commands')
const {basicSetup} = require('codemirror')
const {StreamLanguage} = require('@codemirror/language')
const {jinja2} = require('@codemirror/legacy-modes/mode/jinja2')
const {css} = require('@codemirror/lang-css')
const {html} = require('@codemirror/lang-html')
const {xml} = require('@codemirror/lang-xml')


customElements.define('code-editor', class extends HTMLElement {
    constructor() {
        super()
        this._editorValue = ''
        this._editorLanguage = ''
    }

    get editorValue() {
        return this._editorValue
    }

    set editorValue(value) {
        if (this._editorValue === value) return
        this._editorValue = value
        if (!this._view) return
        this._view.setState(this._createState())
    }

    get editorLanguage() {
        return this._editorLanguage
    }

    set editorLanguage(value) {
        if (this._editorLanguage === value) return
        this._editorLanguage = value
        if (!this._view) return
        this._editorLanguage.setState(this._createState())

    }

    connectedCallback() {
        this._view = new EditorView({
            state: this._createState(),
            parent: this
        })
    }

    _createState() {
        const updateListenerExtension = EditorView.updateListener.of((update) => {
            if (update.docChanged) {
                this._editorValue = update.state.doc.toString()
                this.dispatchEvent(new CustomEvent('editorChanged'))
            }
        })

        const domEventHandlers = EditorView.domEventHandlers({
            "focus": () => {
                this.dispatchEvent(new CustomEvent('focus'))
            }
        })

        const extensions = [
            basicSetup,
            keymap.of(indentWithTab),
            updateListenerExtension,
            domEventHandlers,
        ]

        const languageExtension = this._chooseLanguageExtension()
        if (languageExtension) {
            extensions.push(languageExtension)
        }

        return EditorState.create({
            doc: this._editorValue,
            extensions: extensions
        })
    }

    _chooseLanguageExtension() {
        if (this._editorLanguage === 'jinja2') {
            return StreamLanguage.define(jinja2)
        }
        if (this._editorLanguage === 'css') {
            return css()
        }
        if (this._editorLanguage === 'html') {
            return html()
        }
        if (this._editorLanguage === 'xml') {
            return xml()
        }
        return null
    }
})
