// A markdown field with two ways to write in it: formatted, on tiptap, and the
// markdown source itself.
//
// The existing `markdown-editor` does the same job. What is different here is
// the keyboard, and it is not a detail: every button in the existing toolbar —
// including the one that switches to the markdown source — carries
// `tabIndex = -1`, so none of them can be reached without a pointer. The text
// can be typed either way, so the field is usable; the formatting is not.
//
// The toolbar is therefore the APG toolbar pattern: one tab stop for the whole
// row, arrow keys along it, Home and End to its ends. That is also what stops
// a dozen buttons from standing between the label and the text on the way
// through a form — three stops in total, not fifteen: toolbar, editor, and
// whatever follows.
//
// The other change is that links and images are asked for inline rather than
// through window.prompt(). A prompt cannot be styled, cannot be described, and
// drops a screen reader into a dialog with no relationship to the field it
// came from.

const { Editor } = require('@tiptap/core')
const { StarterKit } = require('@tiptap/starter-kit')
const { Markdown } = require('@tiptap/markdown')
const { Link } = require('@tiptap/extension-link')
const { Image } = require('@tiptap/extension-image')

// Every command in the toolbar, in the order they appear. `active` is what
// decides whether the button reads as pressed, which for a toggle is the whole
// of its state (4.1.2) — the fill alone says nothing to a screen reader.
const COMMANDS = [
    {
        key: 'heading2',
        label: 'Heading 2',
        text: 'H2',
        run: (editor) => editor.chain().focus().toggleHeading({ level: 2 }).run(),
        active: (editor) => editor.isActive('heading', { level: 2 })
    },
    {
        key: 'heading3',
        label: 'Heading 3',
        text: 'H3',
        run: (editor) => editor.chain().focus().toggleHeading({ level: 3 }).run(),
        active: (editor) => editor.isActive('heading', { level: 3 })
    },
    { separator: true },
    {
        key: 'bold',
        label: 'Bold',
        icon: 'fa-bold',
        run: (editor) => editor.chain().focus().toggleBold().run(),
        active: (editor) => editor.isActive('bold')
    },
    {
        key: 'italic',
        label: 'Italic',
        icon: 'fa-italic',
        run: (editor) => editor.chain().focus().toggleItalic().run(),
        active: (editor) => editor.isActive('italic')
    },
    {
        key: 'strike',
        label: 'Strikethrough',
        icon: 'fa-strikethrough',
        run: (editor) => editor.chain().focus().toggleStrike().run(),
        active: (editor) => editor.isActive('strike')
    },
    { separator: true },
    {
        key: 'link',
        label: 'Link',
        icon: 'fa-link',
        prompt: { label: 'Link address', placeholder: 'https://', value: (editor) => editor.getAttributes('link').href || '' },
        run: (editor, href) => {
            if (href) editor.chain().focus().setLink({ href }).run()
            else editor.chain().focus().unsetLink().run()
        },
        active: (editor) => editor.isActive('link')
    },
    {
        key: 'image',
        label: 'Image',
        icon: 'fa-image',
        prompt: { label: 'Image address', placeholder: 'https://', value: () => '' },
        run: (editor, src) => {
            if (src) editor.chain().focus().setImage({ src }).run()
        },
        active: (editor) => editor.isActive('image')
    },
    { separator: true },
    {
        key: 'bulletList',
        label: 'Bullet list',
        icon: 'fa-list-ul',
        run: (editor) => editor.chain().focus().toggleBulletList().run(),
        active: (editor) => editor.isActive('bulletList')
    },
    {
        key: 'orderedList',
        label: 'Numbered list',
        icon: 'fa-list-ol',
        run: (editor) => editor.chain().focus().toggleOrderedList().run(),
        active: (editor) => editor.isActive('orderedList')
    },
    { separator: true },
    {
        key: 'code',
        label: 'Inline code',
        icon: 'fa-code',
        run: (editor) => editor.chain().focus().toggleCode().run(),
        active: (editor) => editor.isActive('code')
    },
    {
        key: 'codeBlock',
        label: 'Code block',
        icon: 'fa-file-code',
        run: (editor) => editor.chain().focus().toggleCodeBlock().run(),
        active: (editor) => editor.isActive('codeBlock')
    },
    {
        key: 'quote',
        label: 'Quote',
        icon: 'fa-quote-left',
        run: (editor) => editor.chain().focus().toggleBlockquote().run(),
        active: (editor) => editor.isActive('blockquote')
    }
]

const MODES = ['rich', 'markdown']

// What the toolbar says, in English. Elm passes the page's language in the
// `words` attribute as JSON with the same keys — the command keys, the prompt
// labels, the two modes, and `toolbar`, which is the whole name of the toolbar
// ("Description formatting") because the order of those words is the
// language's, not ours. Anything missing or unreadable falls back to these.
const DEFAULT_WORDS = {
    heading2: 'Heading 2',
    heading3: 'Heading 3',
    bold: 'Bold',
    italic: 'Italic',
    strike: 'Strikethrough',
    link: 'Link',
    image: 'Image',
    bulletList: 'Bullet list',
    orderedList: 'Numbered list',
    code: 'Inline code',
    codeBlock: 'Code block',
    quote: 'Quote',
    linkPrompt: 'Link address',
    imagePrompt: 'Image address',
    apply: 'Apply',
    cancel: 'Cancel',
    modes: 'Editing mode',
    rich: 'Formatted',
    markdown: 'Markdown',
    toolbar: null
}

function wordsFrom(attribute) {
    let given = {}

    if (attribute) {
        try {
            const parsed = JSON.parse(attribute)
            if (parsed && typeof parsed === 'object') given = parsed
        } catch (error) {
            given = {}
        }
    }

    const words = Object.assign({}, DEFAULT_WORDS)
    for (const key of Object.keys(DEFAULT_WORDS)) {
        if (typeof given[key] === 'string' && given[key] !== '') words[key] = given[key]
    }
    return words
}

// Which item the arrow keys land on next. Exactly one item in a toolbar is
// reachable by Tab; this is what decides which, and it wraps, because a row of
// buttons has no end worth stopping at.
function nextIndex(count, current, key) {
    if (count === 0) return 0

    switch (key) {
        case 'ArrowRight':
            return (current + 1) % count
        case 'ArrowLeft':
            return (current - 1 + count) % count
        case 'Home':
            return 0
        case 'End':
            return count - 1
        default:
            return current
    }
}

function define() {
    class MarkdownEditorElement extends HTMLElement {
        constructor() {
            super()
            this._value = ''
            this._mode = 'rich'
            this._syncing = false
            this._focused = 0
            this.editor = null
        }

        static get observedAttributes() {
            return ['input-id', 'labelledby', 'describedby', 'invalid', 'disabled', 'required', 'label', 'words']
        }

        attributeChangedCallback() {
            this._syncAttributes()
        }

        get editorValue() {
            return this._value
        }

        set editorValue(value) {
            const next = value == null ? '' : String(value)
            if (next === this._value) return

            this._value = next
            this._syncValueIntoViews()
        }

        connectedCallback() {
            if (this.editor) return

            this._build()
            this._createEditor()
            this._syncAttributes()
            this._syncValueIntoViews()
            this._setMode(this._mode, { focus: false })
        }

        disconnectedCallback() {
            if (this.editor) {
                this.editor.destroy()
                this.editor = null
            }

            this.innerHTML = ''
        }

        _build() {
            this.innerHTML =
                '<div class="ui-markdown">' +
                '<div class="ui-markdown__toolbar" role="toolbar"></div>' +
                '<div class="ui-markdown__prompt" hidden></div>' +
                '<div class="ui-markdown__surface"></div>' +
                '<textarea class="ui-markdown__source" spellcheck="false"></textarea>' +
                '</div>'

            this._toolbar = this.querySelector('.ui-markdown__toolbar')
            this._prompt = this.querySelector('.ui-markdown__prompt')
            this._surface = this.querySelector('.ui-markdown__surface')
            this._source = this.querySelector('.ui-markdown__source')

            this._buildToolbar()

            this._source.addEventListener('input', () => {
                if (this._syncing) return
                this._value = this._source.value
                this._emit()
            })

            this._source.addEventListener('blur', () => this.dispatchEvent(new Event('blur', { bubbles: true })))
            this._source.addEventListener('focus', () => this.dispatchEvent(new Event('focus', { bubbles: true })))
        }

        _buildToolbar() {
            this._buttons = []
            this._bubbles = []
            this._separators = []

            for (const command of COMMANDS) {
                if (command.separator) {
                    const separator = document.createElement('span')
                    separator.className = 'ui-markdown__separator'
                    separator.setAttribute('role', 'separator')
                    separator.setAttribute('aria-orientation', 'vertical')
                    this._toolbar.appendChild(separator)
                    this._separators.push(separator)
                    continue
                }

                const button = document.createElement('button')
                button.type = 'button'
                button.className = 'ui-markdown__button'
                button.dataset.command = command.key
                button.setAttribute('aria-pressed', 'false')
                button.tabIndex = -1

                if (command.icon) {
                    button.innerHTML = '<i class="ui-icon fas ' + command.icon + '" aria-hidden="true"></i>'
                } else {
                    button.textContent = command.text
                }

                button.addEventListener('click', () => this._run(command, button))

                // Ui.Tooltip's markup, built by hand because this toolbar is
                // built by hand. The bubble is aria-hidden: the button's
                // aria-label already says the same words, and describing it as
                // well would have a screen reader say them twice.
                //
                // It shows on focus as well as hover, which is what makes it
                // worth having here — the arrow keys move along the row, so
                // the name of each button appears as it is reached.
                const wrapper = document.createElement('span')
                wrapper.className = 'ui-tooltip'

                const bubble = document.createElement('span')
                bubble.className = 'ui-tooltip__bubble ui-tooltip__bubble--below'
                bubble.setAttribute('role', 'tooltip')
                bubble.setAttribute('aria-hidden', 'true')
                bubble.dataset.command = command.key
                this._bubbles.push(bubble)

                wrapper.append(button, bubble)
                this._toolbar.appendChild(wrapper)

                button._wrapper = wrapper
                this._buttons.push(button)
            }

            // The mode switch is a radio group inside the toolbar rather than a
            // pair of buttons: the two are one choice, and a screen reader
            // should say "2 of 2" rather than announcing two unrelated toggles.
            const group = document.createElement('span')
            group.className = 'ui-mode-switch'
            group.setAttribute('role', 'radiogroup')
            this._modeGroup = group

            this._modeButtons = MODES.map((mode) => {
                const button = document.createElement('button')
                button.type = 'button'
                button.className = 'ui-mode-switch__option'
                button.dataset.mode = mode
                button.setAttribute('role', 'radio')
                button.setAttribute('aria-checked', String(mode === this._mode))
                button.tabIndex = -1
                button.addEventListener('click', () => this._setMode(mode, { focus: true }))
                group.appendChild(button)
                return button
            })

            this._toolbar.appendChild(group)
            this._applyWords()
            this._toolbar.addEventListener('keydown', (event) => this._onToolbarKeyDown(event))
            this._setFocusedItem(0)
        }

        // Every focusable thing in the toolbar, in the order it appears —
        // minus whatever the current mode has hidden, which is why it is
        // recomputed rather than kept.
        _items() {
            return this._buttons.concat(this._modeButtons).filter((button) => !button.hidden)
        }

        _setFocusedItem(index) {
            const items = this._items()
            if (items.length === 0) return

            this._focused = Math.max(0, Math.min(index, items.length - 1))
            items.forEach((item, position) => {
                item.tabIndex = position === this._focused ? 0 : -1
            })
        }

        _onToolbarKeyDown(event) {
            if (!['ArrowLeft', 'ArrowRight', 'Home', 'End'].includes(event.key)) return

            const items = this._items()
            const current = items.indexOf(document.activeElement)
            if (current === -1) return

            event.preventDefault()
            const next = nextIndex(items.length, current, event.key)
            this._setFocusedItem(next)
            items[next].focus()
        }

        _createEditor() {
            this.editor = new Editor({
                element: this._surface,
                autofocus: false,
                extensions: [
                    StarterKit.configure({ heading: { levels: [2, 3] }, link: false }),
                    Link.configure({ openOnClick: false }),
                    Image,
                    Markdown
                ],
                content: this._value,
                contentType: 'markdown',
                onUpdate: ({ editor }) => {
                    if (this._syncing || this._mode !== 'rich') return

                    // Only a change of the text is an edit. An update that
                    // leaves the markdown as it was is not sent on, so nothing
                    // but typing marks a form as changed.
                    const next = editor.getMarkdown()
                    if (next === this._value) return

                    this._value = next
                    this._emit()
                },
                onSelectionUpdate: () => this._updatePressedState(),
                onFocus: () => {
                    this._updatePressedState()
                    this.dispatchEvent(new Event('focus', { bubbles: true }))
                },
                onBlur: () => {
                    this._updatePressedState()
                    this.dispatchEvent(new Event('blur', { bubbles: true }))
                }
            })

            // A contenteditable div is a div to everything that is not a
            // pointer. These three say what it is, that it holds more than one
            // line, and which label belongs to it — `for` cannot reach it,
            // because `for` only points at form controls.
            const dom = this.editor.view.dom
            dom.setAttribute('role', 'textbox')
            dom.setAttribute('aria-multiline', 'true')
            dom.classList.add('ui-markdown__content')
        }

        _run(command, button) {
            if (!this.editor) return

            if (command.prompt) {
                this._openPrompt(command, button)
                return
            }

            command.run(this.editor)
            this._afterCommand()
        }

        // Links and images need a value, and asking for it inline keeps the
        // question inside the field it is about: labeled, describable, Escape
        // to abandon, and focus back where it came from either way.
        _openPrompt(command, button) {
            this._closePrompt()

            const id = (this.getAttribute('input-id') || 'ui-markdown') + '-prompt'

            this._prompt.hidden = false
            this._prompt.innerHTML = ''

            const label = document.createElement('label')
            label.className = 'ui-markdown__prompt-label'
            label.setAttribute('for', id)
            const words = wordsFrom(this.getAttribute('words'))
            label.textContent = words[command.key + 'Prompt'] || command.prompt.label

            const input = document.createElement('input')
            input.type = 'url'
            input.id = id
            input.className = 'ui-input'
            input.placeholder = command.prompt.placeholder
            input.value = command.prompt.value(this.editor)

            const apply = document.createElement('button')
            apply.type = 'button'
            apply.className = 'ui-button ui-button--primary ui-button--sm'
            apply.textContent = words.apply

            const cancel = document.createElement('button')
            cancel.type = 'button'
            cancel.className = 'ui-button ui-button--secondary ui-button--sm'
            cancel.textContent = words.cancel

            const done = (value) => {
                this._closePrompt()
                button.focus()

                if (value !== null) {
                    command.run(this.editor, value)
                    this._afterCommand()
                }
            }

            apply.addEventListener('click', () => done(input.value.trim()))
            cancel.addEventListener('click', () => done(null))
            input.addEventListener('keydown', (event) => {
                if (event.key === 'Enter') {
                    event.preventDefault()
                    done(input.value.trim())
                } else if (event.key === 'Escape') {
                    event.preventDefault()
                    done(null)
                }
            })

            this._prompt.append(label, input, apply, cancel)
            input.focus()
            input.select()
        }

        _closePrompt() {
            this._prompt.hidden = true
            this._prompt.innerHTML = ''
        }

        _afterCommand() {
            this._value = this.editor.getMarkdown()
            this._updatePressedState()
            this._emit()
        }

        _updatePressedState() {
            if (!this.editor) return

            for (const button of this._buttons) {
                const command = COMMANDS.find((entry) => entry.key === button.dataset.command)
                button.setAttribute('aria-pressed', String(Boolean(command && command.active(this.editor))))
            }
        }

        _setMode(mode, options) {
            this._mode = MODES.includes(mode) ? mode : 'rich'

            const isMarkdown = this._mode === 'markdown'

            this._closePrompt()
            this._surface.hidden = isMarkdown
            this._source.hidden = !isMarkdown

            // The formatting commands belong to the formatted view. In the
            // source view they would act on something that is not on screen.
            for (const button of this._buttons) {
                button.hidden = isMarkdown
                // The wrapper too, or the tooltip's empty box keeps the space.
                button._wrapper.hidden = isMarkdown
            }

            // And what divides them, or the dividers are left standing alone.
            for (const separator of this._separators) {
                separator.hidden = isMarkdown
            }

            for (const button of this._modeButtons) {
                button.setAttribute('aria-checked', String(button.dataset.mode === this._mode))
            }

            this._syncValueIntoViews()
            this._setFocusedItem(isMarkdown ? 0 : this._focused)

            if (options && options.focus) {
                const target = isMarkdown ? this._source : this.editor && this.editor.view.dom
                if (target) target.focus()
            }
        }

        _syncValueIntoViews() {
            this._syncing = true

            if (this._source && this._source.value !== this._value) {
                this._source.value = this._value
            }

            if (this.editor && this.editor.getMarkdown() !== this._value) {
                // Putting the model's value in is not an edit, so it emits no
                // update at all — not only one that `_syncing` would drop.
                this.editor.commands.setContent(this._value, { contentType: 'markdown', emitUpdate: false })
            }

            this._syncing = false
        }

        _syncAttributes() {
            if (!this._surface) return

            const id = this.getAttribute('input-id')
            const labelledby = this.getAttribute('labelledby')
            const describedby = this.getAttribute('describedby')
            const invalid = this.hasAttribute('invalid')
            const disabled = this.hasAttribute('disabled')
            this._applyWords()

            if (id) {
                this._source.id = id
                this._source.name = id
            }

            for (const node of [this._source, this.editor && this.editor.view.dom]) {
                if (!node) continue

                if (labelledby) node.setAttribute('aria-labelledby', labelledby)
                if (describedby) node.setAttribute('aria-describedby', describedby)
                else node.removeAttribute('aria-describedby')

                if (invalid) node.setAttribute('aria-invalid', 'true')
                else node.removeAttribute('aria-invalid')

                if (this.hasAttribute('required')) node.setAttribute('aria-required', 'true')
                else node.removeAttribute('aria-required')
            }

            this._source.disabled = disabled
            this.querySelector('.ui-markdown').classList.toggle('is-invalid', invalid)
            this.querySelector('.ui-markdown').classList.toggle('is-disabled', disabled)

            // Without `false` TipTap reports the switch as an update of the
            // content, the update handler reports it to Elm as an edit, and
            // every form with a markdown field was "changed" the moment it
            // loaded — this runs as the element connects, and again whenever an
            // attribute such as `invalid` changes.
            if (this.editor) this.editor.setEditable(!disabled, false)

            for (const button of this._buttons.concat(this._modeButtons)) {
                button.disabled = disabled
            }
        }

        // Every name in the toolbar, from the `words` attribute. Run when the
        // toolbar is built and whenever the attributes change, so a page whose
        // language arrives after the field does still speak it.
        _applyWords() {
            if (!this._toolbar || !this._buttons) return

            const words = wordsFrom(this.getAttribute('words'))
            const label = this.getAttribute('label') || 'Editor'

            this._toolbar.setAttribute('aria-label', words.toolbar || label + ' formatting')

            for (const button of this._buttons) {
                const command = COMMANDS.find((entry) => entry.key === button.dataset.command)
                button.setAttribute('aria-label', words[button.dataset.command] || (command && command.label) || '')
            }

            for (const bubble of this._bubbles) {
                const command = COMMANDS.find((entry) => entry.key === bubble.dataset.command)
                bubble.textContent = words[bubble.dataset.command] || (command && command.label) || ''
            }

            if (this._modeGroup) this._modeGroup.setAttribute('aria-label', words.modes)

            for (const button of this._modeButtons || []) {
                button.textContent = words[button.dataset.mode]
            }
        }

        _emit() {
            this.dispatchEvent(new CustomEvent('editorChanged', { bubbles: true }))
        }
    }

    customElements.define('ui-markdown-editor', MarkdownEditorElement)
}

if (typeof customElements !== 'undefined' && typeof HTMLElement !== 'undefined') {
    define()
}

module.exports = { COMMANDS, MODES, DEFAULT_WORDS, nextIndex, wordsFrom }
