// Style rules for the design system, enforced without a new dependency.
//
// These are the checks a stylelint configuration would hold. They
// live here because adding stylelint would mean touching package-lock and
// node_modules; when stylelint is adopted these move into .stylelintrc and this
// file goes away.

const fs = require('fs')
const path = require('path')

// The design system, and everything built on it that must follow the same rules.
const ROOTS = ['packages/elm-ui/scss', 'packages/elm-common/scss', 'apps/dsw/scss', 'apps/registry/scss']
const ELM_ROOTS = ['packages/elm-ui/elm', 'packages/elm-common/elm', 'apps/dsw/elm', 'apps/registry/elm']
// `_brand.scss` is an application's brand override (the dsw app's): role values
// derived by Ui.Theme.Derive, so literal colors by nature, like _roles.scss.
const TOKEN_FILES = ['_tokens.scss', '_roles.scss', '_brand.scss']

const RULES = [
    {
        name: 'no literal colors outside tokens and roles',
        skip: (file) => TOKEN_FILES.includes(path.basename(file)),
        pattern: /#[0-9a-fA-F]{3,8}\b/,
        hint: 'colors are roles — use var(--ui-*)'
    },
    {
        name: 'no outline: none',
        skip: () => false,
        pattern: /outline:\s*(none|0)\b/,
        hint: 'the focus ring is built on outline; removing it removes the ring',

        // Allowed where the ring is *moved* rather than removed: a widget whose
        // inner element takes the focus but whose whole box is the control, so
        // the ring belongs on the box. The reason has to say where it went, on
        // one of the lines above, so the exemption is a decision somebody made
        // rather than a way round the rule.
        allowWithReason: /WCAG 2\.4\.7 ring moved:/
    },
    {
        name: 'logical properties only',
        skip: () => false,
        pattern: /^\s*(margin|padding|border)-(left|right)\s*:|^\s*(left|right)\s*:/,
        hint: 'use margin-inline / padding-inline / inset-inline so RTL stays a stylesheet concern'
    },
    {
        name: 'no CSS reordering',
        skip: () => false,
        pattern: /^\s*order\s*:|flex-direction:\s*\w+-reverse/,
        hint: 'DOM order is focus order — reorder the DOM, not the CSS'
    },
    {
        name: 'only opacity and transform animate',
        skip: () => false,
        pattern: /transition:[^;]*\b(width|height|top|left|right|bottom|margin|padding)\b/,
        hint: 'animating layout properties forces layout every frame'
    }
]

function walkAll(roots, walker) {
    return roots.filter((root) => fs.existsSync(root)).flatMap(walker)
}

function walkElm(dir) {
    return fs.readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
        const full = path.join(dir, entry.name)
        return entry.isDirectory() ? walkElm(full) : full.endsWith('.elm') ? [full] : []
    })
}

function walk(dir) {
    return fs.readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
        const full = path.join(dir, entry.name)
        return entry.isDirectory() ? walk(full) : full.endsWith('.scss') ? [full] : []
    })
}

const failures = []

// WCAG 2.5.2: no part of an action may run on the down-event, so that pressing
// and sliding away cancels. The one legitimate use of mousedown is preventing
// the default focus shift, which is why the check is for a *handler* that does
// something rather than for the word.
for (const file of walkAll(ELM_ROOTS, walkElm)) {
    const lines = fs.readFileSync(file, 'utf8').split('\n')

    lines.forEach((line, index) => {
        if (/^\s*(--|\{-)/.test(line)) return
        if (!/mousedown|onMouseDown|pointerdown/i.test(line)) return

        // Allowed: preventDefaultOn "mousedown" that dispatches Noop.
        if (/preventDefaultOn "mousedown"[^)]*Noop/.test(line)) return

        // Allowed: a press that is essential to the function, which 2.5.2
        // itself carves out — a drag begins when the button goes down and
        // there is no other moment for it to begin. The reason has to be
        // written on the line above, so the exemption is a decision somebody
        // made rather than a habit.
        const reason = lines.slice(Math.max(0, index - 4), index).join('\n')
        if (/--\s*WCAG 2\.5\.2 down-event:/.test(reason)) return

        failures.push(
            `${file}:${index + 1}  actions run on release, not press (WCAG 2.5.2)\n    ${line.trim()}`
        )
    })
}

for (const file of walkAll(ROOTS, walk)) {
    const lines = fs.readFileSync(file, 'utf8').split('\n')

    lines.forEach((line, index) => {
        // Comments explain the rules; they are not violations of them.
        if (/^\s*\/\//.test(line)) return

        for (const rule of RULES) {
            if (rule.skip(file)) continue
            if (!rule.pattern.test(line)) continue

            if (rule.allowWithReason) {
                // Far enough back to sit above the selector as well as the
                // declaration, which is where a reason about a rule belongs.
                const reason = lines.slice(Math.max(0, index - 8), index).join('\n')
                if (rule.allowWithReason.test(reason)) continue
            }

            failures.push(`${file}:${index + 1}  ${rule.name} — ${rule.hint}\n    ${line.trim()}`)
        }
    })
}

// Every var(--ui-*) resolves to something. A name with no declaration and no
// fallback is not an error to any tool in this repo — the declaration is simply
// dropped and the property inherits, which looks like a styling choice. This
// caught --ui-text-subtle, a role that was written about but never defined.
{
    const files = walkAll(ROOTS, walk)
    const declared = new Set()

    for (const file of files) {
        const css = fs.readFileSync(file, 'utf8')
        for (const match of css.matchAll(/(--ui-[a-z0-9-]+)\s*:/g)) {
            declared.add(match[1])
        }
    }

    for (const file of files) {
        const lines = fs.readFileSync(file, 'utf8').split('\n')

        lines.forEach((line, index) => {
            if (/^\s*\/\//.test(line)) return

            // A comma after the name means a fallback follows, which is how a
            // property set from Elm is referenced.
            for (const match of line.matchAll(/var\(\s*(--ui-[a-z0-9-]+)\s*([,)])/g)) {
                if (match[2] === ',') continue
                if (declared.has(match[1])) continue

                failures.push(
                    `${file}:${index + 1}  ${match[1]} is never defined and has no fallback\n    ${line.trim()}`
                )
            }
        })
    }
}

// No class belongs to two components. .ui-code was inline code in
// primitives/_typography.scss and the CodeMirror wrapper in
// components/_code-editor.scss, so a few words of code in a sentence were given
// the editor's border, radius and overflow — valid CSS, no warning anywhere, and
// only visible if you happened to look at the right paragraph.
{
    const owners = new Map()

    for (const file of walkAll(ROOTS, walk)) {
        const css = fs.readFileSync(file, 'utf8')

        // Top-level rules only. An indented one is a component styling
        // another's class inside its own context — `.ui-button` inside
        // `.ui-button-group`, say — which is a reference, not a second owner.
        for (const match of css.matchAll(/^(\.[a-z][a-z0-9_-]*)[\s,{:.]/gm)) {
            const name = match[1]
            if (!owners.has(name)) owners.set(name, new Set())
            owners.get(name).add(file)
        }
    }

    for (const [name, files] of owners) {
        if (files.size < 2) continue

        failures.push(
            `${name} is defined in ${files.size} files — a class belongs to one component\n    ${[...files].join('\n    ')}`
        )
    }
}

if (failures.length > 0) {
    console.error(`ui style rules: ${failures.length} violation(s)\n`)
    failures.forEach((failure) => console.error(failure + '\n'))
    process.exit(1)
}

console.log('ui style rules OK')
