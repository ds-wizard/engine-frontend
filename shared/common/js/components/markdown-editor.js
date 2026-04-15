const {Editor} = require('@tiptap/core')
const StarterKitModule = require('@tiptap/starter-kit')
const MarkdownModule = require('@tiptap/markdown')

const LinkModule = require('@tiptap/extension-link')
const ImageModule = require('@tiptap/extension-image')
const TableModule = require('@tiptap/extension-table')
const TableRowModule = require('@tiptap/extension-table-row')
const TableCellModule = require('@tiptap/extension-table-cell')
const TableHeaderModule = require('@tiptap/extension-table-header')


function normalizeExtension(mod) {
    if (!mod) return null

    if (mod.default) return mod.default

    // common named exports
    if (mod.Markdown) return mod.Markdown
    if (mod.Table) return mod.Table

    // If module itself *looks* like an extension, return it
    if (typeof mod === 'function' || (typeof mod === 'object' && mod.name)) return mod

    // Fallback: pick the first exported property that looks like an
    // extension (function or object). This handles modules that export
    // multiple items (e.g. { Table, TableRow }).
    if (typeof mod === 'object') {
        const keys = Object.keys(mod)
        for (let i = 0; i < keys.length; i++) {
            const v = mod[keys[i]]
            if (!v) continue
            if (typeof v === 'function') return v
            if (typeof v === 'object' && v.name) return v
        }
    }

    return null
}

const StarterKit = normalizeExtension(StarterKitModule)
const Markdown = normalizeExtension(MarkdownModule)
const Link = normalizeExtension(LinkModule)
const Image = normalizeExtension(ImageModule)
const Table = normalizeExtension(TableModule)
const TableRow = normalizeExtension(TableRowModule)
const TableCell = normalizeExtension(TableCellModule)
const TableHeader = normalizeExtension(TableHeaderModule)

const DEFAULT_LABELS = {
    heading2: 'Heading 2',
    heading3: 'Heading 3',
    bold: 'Bold',
    italic: 'Italic',
    strike: 'Strikethrough',
    link: 'Link',
    image: 'Image',
    bulletList: 'Bullet List',
    orderedList: 'Ordered List',
    code: 'Inline Code',
    codeBlock: 'Code Block',
    richText: 'Rich Text',
    markdown: 'Markdown',
}

class MarkdownEditorElement extends HTMLElement {
    constructor() {
        super()

        this._editorValue = ''
        this._editorMode = 'rich'
        this._connected = false
        this._syncingFromEditor = false
        this._suppressAutoFocus = false

        this.editor = null
        this.container = null
        this.toolbar = null
        this.toolbarLeft = null
        this.toolbarRight = null
        this.textarea = null
        this.richHost = null
        this._labelsAssigned = false
        this._labels = Object.assign({}, DEFAULT_LABELS)
        this._previousEditorMode = null
    }

    static get observedAttributes() {
        return ['labels']
    }

    attributeChangedCallback(name, oldValue, newValue) {
        if (name === 'labels' && oldValue !== newValue) {
            try {
                this.labels = newValue ? JSON.parse(newValue) : {}
            } catch (e) {
                // if parsing fails, ignore and keep defaults
                // but still attempt to notify consumer via console
                console.warn('markdown-editor: failed to parse labels attribute JSON', e)
            }
        }
    }

    set labels(obj) {
        if (this._labelsAssigned) return

        this._labels = Object.assign({}, DEFAULT_LABELS, obj || {})
        this._labelsAssigned = true

        // Re-render UI labels if already connected
        if (this._connected) {
            try {
                this.renderToolbar()
                this.renderModeToggle()
                this.syncValueIntoUi()
                this.syncModeIntoUi()
            } catch (e) {
                // ignore UI update errors
            }
        }
    }

    get labels() {
        return this._labels
    }

    getLabel(key) {
        return (this._labels && this._labels[key]) || DEFAULT_LABELS[key] || String(key)
    }

    get editorValue() {
        return this._editorValue
    }

    set editorValue(value) {
        const next = value == null ? '' : String(value)
        if (next === this._editorValue) return

        this._editorValue = next

        if (this._connected) {
            this.syncValueIntoUi()
        }
    }

    get editorMode() {
        return this._editorMode
    }

    set editorMode(value) {
        const next = value === 'markdown' ? 'markdown' : 'rich'
        if (next === this._editorMode) return

        // remember previous mode so transitions can be handled specially
        this._previousEditorMode = this._editorMode
        this._editorMode = next

        if (this._connected) {
            this.syncModeIntoUi()
            this.renderModeToggle()
        }
    }

    connectedCallback() {
        if (this._connected) return
        this._connected = true

        this.render()
        this._suppressAutoFocus = true
        this.createEditor()
        this.renderToolbar()
        this.syncValueIntoUi()
        this.syncModeIntoUi()
    }

    disconnectedCallback() {
        if (this.editor) {
            this.editor.destroy()
            this.editor = null
        }

        // Remove container click handler to avoid leaks
        try {
            if (this._containerClickHandler && this.container) {
                this.container.removeEventListener('click', this._containerClickHandler)
            }
        } catch (e) {
            // ignore
        }
        this._containerClickHandler = null

        this._connected = false
    }

    render() {
        if (this.container) return

        this.innerHTML = `
      <div class="md-editor">
        <div class="md-editor__toolbar">
          <div class="md-editor__toolbar-left"></div>
          <div class="md-editor__toolbar-right"></div>
        </div>
        <textarea class="md-editor__textarea" spellcheck="false" autocomplete="off" autocorrect="off" autocapitalize="off" inputmode="text" data-gramm="false"></textarea>
        <div class="md-editor__surface"></div>
      </div>
    `

        this.container = this.querySelector('.md-editor')
        this.toolbar = this.querySelector('.md-editor__toolbar')
        this.toolbarLeft = this.querySelector('.md-editor__toolbar-left')
        this.toolbarRight = this.querySelector('.md-editor__toolbar-right')
        this.textarea = this.querySelector('.md-editor__textarea')
        this.richHost = this.querySelector('.md-editor__surface')

        // Clicking the editor surface (outside toolbar/textarea) should
        // focus the TipTap editor so the user can type. Attach a
        // container-level click handler that focuses the editor but
        // ignores clicks on toolbar controls or the textarea.
        if (!this._containerClickHandler) {
            this._containerClickHandler = (ev) => {
                try {
                    if (!this.editor) return
                    const t = ev.target
                    // ignore toolbar interactions
                    if (this.toolbar && this.toolbar.contains && this.toolbar.contains(t)) return
                    if (this.textarea && this.textarea.contains && this.textarea.contains(t)) return

                    // Focus the editor using whichever API is available.
                    try {
                        if (this.editor.chain && typeof this.editor.chain === 'function') {
                            this.editor.chain().focus().run()
                            return
                        }
                    } catch (e) {
                        // ignore
                    }

                    try {
                        if (this.editor.commands && typeof this.editor.commands.focus === 'function') {
                            this.editor.commands.focus()
                            return
                        }
                    } catch (e) {
                        // ignore
                    }

                    try {
                        if (this.editor.view && this.editor.view.dom && typeof this.editor.view.dom.focus === 'function') {
                            this.editor.view.dom.focus()
                        }
                    } catch (e) {
                        // ignore
                    }
                } catch (e) {
                    // ignore
                }
            }

            try { this.container.addEventListener('click', this._containerClickHandler) } catch (e) {}
        }

        if (!this._modeWrapper) {
            this._modeWrapper = document.createElement('div')
            this._modeWrapper.className = 'md-editor__mode-toggle'

            this._richModeButton = document.createElement('button')
            this._richModeButton.type = 'button'
            this._richModeButton.className = 'md-editor__mode-button'
            this._richModeButton.tabIndex = -1
            this._richModeButton.dataset.cy = 'md-editor_richtext'

            this._markdownModeButton = document.createElement('button')
            this._markdownModeButton.type = 'button'
            this._markdownModeButton.className = 'md-editor__mode-button'
            this._markdownModeButton.tabIndex = -1
            this._markdownModeButton.dataset.cy = 'md-editor_markdown'

            this._modeWrapper.appendChild(this._richModeButton)
            this._modeWrapper.appendChild(this._markdownModeButton)

            this._richModeButton.addEventListener('click', () => {
                if (this._editorMode === 'rich') return
                this.editorMode = 'rich'
                this.emitModeChange()
            })

            this._markdownModeButton.addEventListener('click', () => {
                if (this._editorMode === 'markdown') return
                this.editorMode = 'markdown'
                this.emitModeChange()
            })
        }

        if (this.toolbarRight && !this.toolbarRight.contains(this._modeWrapper)) {
            this.toolbarRight.appendChild(this._modeWrapper)
        }

        this.textarea.addEventListener('input', () => {
            this._editorValue = this.textarea.value
            this.emitChange()
        })

        this.textarea.addEventListener('blur', () => {
            if (!this.editor) return
            try {
                this._syncingFromEditor = true
                const current = this.editor.getMarkdown()
                if (current !== this._editorValue) {
                    this.editor.commands.setContent(this._editorValue, {
                        contentType: 'markdown',
                    })
                    this.updateToolbarState()
                    this.emitChange()
                }
            } catch (e) {
                console.error(e)
                // ignore sync errors
            } finally {
                this._syncingFromEditor = false
            }
        })

        this.textarea.addEventListener('focus', () => {
            this.dispatchEvent(new Event('focus', {bubbles: true}))
        })
    }

    createEditor() {
        this._syncingFromEditor = true
        this.editor = new Editor({
            element: this.richHost,
            autofocus: false,
            // Keep the editor editable so user clicks can focus and type
            editable: true,
            extensions: [
                // Use StarterKit but disable its built-in link so we can add
                // the Link extension explicitly (avoids duplicate link mark)
                StarterKit.configure({
                    heading: {levels: [2, 3]},
                    link: false,
                }),
                // Add link/image/table extensions explicitly
                Link,
                Image,
                // Table extensions: configure resizable if supported
                Table && Table.configure ? Table.configure({resizable: true}) : Table,
                TableRow,
                TableHeader,
                TableCell,
                Markdown,
            ],
            content: this._editorValue,
            contentType: 'markdown',
            onFocus: ({editor}) => {
                // Always notify consumers when the editor gains focus.
                // We intentionally do not forcibly blur here because user
                // clicks should focus the editor even during init. Initial
                // autofocus is already disabled and editability is managed
                // separately.
                try {
                    this.dispatchEvent(new Event('focus', {bubbles: true}))
                    // Refresh toolbar state when editor gets focus so active
                    // buttons only show while focused.
                    try { this.updateToolbarState() } catch (e) {}
                } catch (e) {
                    // ignore
                }
            },
            onBlur: ({editor}) => {
                // Refresh toolbar state on blur so active indicators are removed
                try { this.updateToolbarState() } catch (e) {}
            },
            onSelectionUpdate: () => {
                this.updateToolbarState()
            },
            onUpdate: ({editor}) => {
                if (this._editorMode === 'markdown') return

                // If we're currently performing a programmatic sync into
                // the editor (setContent), avoid treating the resulting
                // onUpdate as a user edit. Also only treat updates that
                // occur while the editor is focused as user edits — this
                // prevents emitting when the value is changed from outside.
                if (this._syncingFromEditor) return
                if (!editor.isFocused) return

                const markdown = editor.getMarkdown()

                this._syncingFromEditor = true
                this._editorValue = markdown

                if (this.textarea && this.textarea.value !== markdown) {
                    this.textarea.value = markdown
                }

                this.updateToolbarState()
                this.emitChange()

                this._syncingFromEditor = false
            },
        })
        // Clear the initialization suppression on the next tick so
        // subsequent user edits will emit change events.
        setTimeout(() => {
            this._syncingFromEditor = false
        }, 0)

        // Ensure the editor DOM can receive focus by setting a tabindex
        // and making the DOM contenteditable true when possible. This is a
        // best-effort attempt that works across TipTap versions.
        try {
            if (this.editor && this.editor.view && this.editor.view.dom) {
                try {
                    const dom = this.editor.view.dom
                    if (typeof dom.setAttribute === 'function') {
                        dom.setAttribute('tabindex', '0')
                    }
                    if (typeof dom.setAttribute === 'function') {
                        dom.setAttribute('contenteditable', 'true')
                    }
                    if (this.editor && typeof this.editor.setEditable === 'function') {
                        try { this.editor.setEditable(true) } catch (e) {}
                    }
                } catch (e) {
                    // ignore
                }
            }
        } catch (e) {
            // ignore
        }
    }

    renderToolbar() {
        if (!this.toolbarLeft) return

        this.toolbarLeft.innerHTML = ''

        this.addButton('H2', 'heading2', () => {
            this.editor.chain().focus().toggleHeading({level: 2}).run()
        }, this.getLabel('heading2'))

        this.addButton('H3', 'heading3', () => {
            this.editor.chain().focus().toggleHeading({level: 3}).run()
        }, this.getLabel('heading3'))

        this.addButton('<i class="fas fa-fw fa-bold"></i>', 'bold', () => {
            this.editor.chain().focus().toggleBold().run()
        }, this.getLabel('bold'))

        this.addButton('<i class="fas fa-fw fa-italic"></i>', 'italic', () => {
            this.editor.chain().focus().toggleItalic().run()
        }, this.getLabel('italic'))

        this.addButton('<i class="fas fa-fw fa-strikethrough"></i>', 'strike', () => {
            this.editor.chain().focus().toggleStrike().run()
        }, this.getLabel('strike'))

        this.addButton('<i class="fas fa-fw fa-link"></i>', 'link', () => {
            if (!this.editor) return
            const href = window.prompt('Enter URL', 'https://')
            if (!href) return

            try {
                // setLink will add a link mark to the selection or position.
                // Pass title when available; many TipTap Link implementations
                // accept it as an attribute.
                this.editor.chain().focus().setLink({ href }).run()
            } catch (e) {
                // Some TipTap versions use toggleLink / updateMark APIs; try a safe fallback
                try {
                    this.editor.chain().focus().toggleLink({href}).run()
                } catch (err) {
                    // ignore if not supported
                }
            }
        }, this.getLabel('link'))

        this.addButton('<i class="fas fa-fw fa-image"></i>', 'image', () => {
            if (!this.editor) return
            const src = window.prompt('Enter image URL', 'https://')
            if (!src) return
            const alt = window.prompt('Alt text (optional)', '') || ''
            try {
                this.editor.chain().focus().setImage({src, alt}).run()
            } catch (e) {
                // fallback: insert an HTML image if setImage not available
                try {
                    this.editor.commands.insertContent(`<img src="${src}" alt="${alt}" />`)
                } catch (err) {
                    // ignore
                }
            }
        }, this.getLabel('image'))

        this.addButton('<i class="fas fa-fw fa-list-ul"></i>', 'bulletList', () => {
            this.editor.chain().focus().toggleBulletList().run()
        }, this.getLabel('bulletList'))

        this.addButton('<i class="fas fa-fw fa-list-ol"></i>', 'orderedList', () => {
            this.editor.chain().focus().toggleOrderedList().run()
        }, this.getLabel('orderedList'))

        this.addButton('<i class="fas fa-fw fa-code"></i>', 'code', () => {
            this.editor.chain().focus().toggleCode().run()
        }, this.getLabel('code'))

        this.addButton('<i class="far fa-fw fa-file-code"></i>', 'codeBlock', () => {
            this.editor.chain().focus().toggleCodeBlock().run()
        }, this.getLabel('codeBlock'))

        this.renderModeToggle()
        this.updateToolbarState()

        // Ensure the persistent mode toggle is attached to toolbarRight so
        // it remains available even when toolbar content is re-rendered.
        if (this.toolbarRight && this._modeWrapper && !this.toolbarRight.contains(this._modeWrapper)) {
            this.toolbarRight.appendChild(this._modeWrapper)
        }
    }

    renderModeToggle() {
        if (!this._richModeButton || !this._markdownModeButton) return

        this._richModeButton.textContent = this.getLabel('richText')
        this._markdownModeButton.textContent = this.getLabel('markdown')

        this._richModeButton.classList.toggle('is-active', this._editorMode === 'rich')
        this._markdownModeButton.classList.toggle('is-active', this._editorMode === 'markdown')
    }

    addButton(label, key, onClick, readableLabel) {
        const button = document.createElement('button')
        button.type = 'button'
        button.className = 'md-editor__button with-tooltip'
        button.tabIndex = -1
        button.innerHTML = label
        try {
            const plain = (label || '').replace(/<[^>]+>/g, '').trim()
            const aria = (readableLabel && String(readableLabel).trim()) || plain || key || 'button'
            button.setAttribute('aria-label', aria)
            button.setAttribute('data-tooltip', aria)
        } catch (e) {
            // ignore
        }
        button.dataset.command = key

        button.addEventListener('click', () => {
            if (!this.editor) return
            onClick()
            this.updateToolbarState()
            // Ensure outside listeners are notified when toolbar actions
            // mutate the document. Update our internal value from the
            // editor (prefer markdown if available) before emitting so
            // emitChange() compares the right content.
            // Defer reading the editor state to the next tick so any
            // document updates performed by the command have been applied.
            // This ensures emitChange() observes the new content.
            try {
                setTimeout(() => {
                    try {
                        if (this.editor && typeof this.editor.getMarkdown === 'function') {
                            try {
                                this._editorValue = this.editor.getMarkdown()
                            } catch (e) {
                                // ignore
                            }
                        }
                        // Dispatch the change event immediately so external
                        // listeners are notified right after toolbar actions.
                        try {
                            this.dispatchEvent(new Event('editorChanged', { bubbles: true }))
                        } catch (e) {
                            // ignore
                        }
                    } catch (e) {
                        // ignore
                    }
                }, 0)
            } catch (e) { /* ignore */ }
        })

        this.toolbarLeft.appendChild(button)
    }

    updateToolbarState() {
        if (!this.editor || !this.toolbarLeft) return
        // Determine whether the editor is focused. Support different
        // TipTap / ProseMirror APIs and fall back to checking the active
        // element against the editor DOM.
        let editorFocused = false
        try {
            if (this.editor && typeof this.editor.isFocused === 'function') {
                editorFocused = this.editor.isFocused()
            } else if (this.editor && this.editor.view && typeof this.editor.view.hasFocus === 'function') {
                editorFocused = this.editor.view.hasFocus()
            } else if (this.editor && this.editor.view && this.editor.view.dom) {
                try {
                    editorFocused = this.editor.view.dom === document.activeElement || (this.editor.view.dom.contains && this.editor.view.dom.contains(document.activeElement))
                } catch (e) {
                    editorFocused = false
                }
            }
        } catch (e) {
            editorFocused = false
        }

        const buttons = this.toolbarLeft.querySelectorAll('[data-command]')

        for (let i = 0; i < buttons.length; i++) {
            const button = buttons[i]
            const key = button.dataset.command
            let active = false

            if (key === 'heading2') {
                active = this.editor.isActive('heading', {level: 2})
            } else if (key === 'heading3') {
                active = this.editor.isActive('heading', {level: 3})
            } else if (key === 'bold') {
                active = this.editor.isActive('bold')
            } else if (key === 'italic') {
                active = this.editor.isActive('italic')
            } else if (key === 'strike') {
                active = this.editor.isActive('strike')
            } else if (key === 'bulletList') {
                active = this.editor.isActive('bulletList')
            } else if (key === 'orderedList') {
                active = this.editor.isActive('orderedList')
            } else if (key === 'code') {
                active = this.editor.isActive('code')
            } else if (key === 'codeBlock') {
                active = this.editor.isActive('codeBlock')
            } else if (key === 'link') {
                active = this.editor.isActive('link')
            } else if (key === 'image') {
                // image is active if the selection includes an image node
                active = this.editor.isActive('image')
            }

            // Only show active state when the editor is focused
            button.classList.toggle('is-active', !!editorFocused && !!active)
        }
    }

    syncValueIntoUi() {
        if (!this.textarea) return

        if (this.textarea.value !== this._editorValue) {
            this.textarea.value = this._editorValue
        }

        if (this.editor && !this._syncingFromEditor) {
            const current = this.editor.getMarkdown()

            if (current !== this._editorValue) {
                this._syncingFromEditor = true
                try {
                    this.editor.commands.setContent(this._editorValue, {
                        contentType: 'markdown',
                    })
                } catch (e) {
                    // ignore
                } finally {
                    this._syncingFromEditor = false
                }
                this.updateToolbarState()
            }
        }
    }

    syncModeIntoUi() {
        const isMarkdown = this._editorMode === 'markdown'
        const enteringRich = this._previousEditorMode === 'markdown' && !isMarkdown

        if (this.textarea) {
            this.textarea.style.display = isMarkdown ? 'block' : 'none'
        }

        if (this.richHost) {
            this.richHost.style.display = isMarkdown ? 'none' : 'block'
        }

        // Hide toolbar buttons (left side) when in markdown mode but keep the
        // mode toggle (right side) visible so the user can switch back.
        if (this.toolbarLeft) {
            this.toolbarLeft.style.display = isMarkdown ? 'none' : ''
        }

        if (isMarkdown) {
            if (this.textarea) {
                // update textarea content
                this.textarea.value = this._editorValue

                // Only move focus/caret to the end when *entering* markdown mode
                // from elsewhere. If the textarea is already focused (user is
                // actively typing), preserve the current selection/caret so we
                // don't jump to the end unexpectedly (e.g. when pressing Enter).
                try {
                    if (document.activeElement !== this.textarea) {
                        this.textarea.focus()
                        // const len = this.textarea.value ? this.textarea.value.length : 0
                        this.textarea.selectionStart = this.textarea.selectionEnd = 0
                    }
                } catch (e) {
                    // ignore any focus/selection errors (e.g. not attached to DOM yet)
                }
            }
        } else if (this.editor) {
            // Defer setContent & focus to the next tick so the editor DOM is
            // visible and TipTap can properly focus. Calling these while the
            // editor is still hidden can cause focus/selection oddities and
            // may fail to apply correctly in some browsers/build setups.
            const doSyncToEditor = () => {
                try {
                    const current = this.editor.getMarkdown()
                    if (current !== this._editorValue) {
                        this._syncingFromEditor = true
                        try {
                            this.editor.commands.setContent(this._editorValue, {
                                contentType: 'markdown',
                            })
                        } catch (e) {
                            // ignore
                        } finally {
                            this._syncingFromEditor = false
                        }
                    }
                } catch (e) {
                    // ignore sync errors
                    console.error(e)
                }

                this.updateToolbarState()

                // If we were suppressing auto-focus for initial mount, clear
                // the flag now that the deferred sync has run. This ensures
                // we don't prematurely unset suppression before the
                // scheduled focus occurs.
                if (this._suppressAutoFocus) {
                    this._suppressAutoFocus = false
                }
            }

            // Use a small timeout to ensure DOM visibility changes have taken
            // effect before interacting with TipTap.
            setTimeout(doSyncToEditor, 0)
        }
    }

    emitChange() {
        try {
            const normalized = normalizeEditorValue(this._editorValue)
            console.log('trying to emit', normalized === this._lastEmittedNormalized)
            if (normalized === this._lastEmittedNormalized) return
            this._lastEmittedNormalized = normalized
        } catch (e) {
            // If normalization fails for any reason, fall back to emitting
            // the event to avoid silently dropping changes.
        }

        this.dispatchEvent(new Event('editorChanged', {bubbles: true}))
    }

    emitModeChange() {
        this.dispatchEvent(new Event('editorModeChanged', {bubbles: true}))
    }
}

if (!customElements.get('markdown-editor')) {
    customElements.define('markdown-editor', MarkdownEditorElement)
}

module.exports = MarkdownEditorElement