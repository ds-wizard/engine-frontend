const _ = require('lodash');
const {execSync} = require('child_process')
const fs = require('fs')
const glob = require('glob')
const {component: findComponent} = require('../build/components')

const componentDir = (name) => findComponent(name).dir


const regexGettext = /[\s+(]gettext "(.*?[^\\])?(\\\\)*"/g
const regexNGettext = /[\s+(]ngettext \( "(.*?[^\\])?(\\\\)*", "(.*?[^\\])?(\\\\)*" \)/g

// npm install po2json@1.0.0-beta
// npx po2json -f jed cs.po cs.json

// `COMPONENT=dsw node tools/i18n/create-pot-file.js` writes locale/dsw.pot. The
// webpack build of dsw ships the same content as `ui.pot` beside the
// application (tools/build/pot-plugin.js), which Settings → Locales downloads.
if (require.main === module) {
    const component = process.env.COMPONENT
    fs.mkdirSync('locale', {recursive: true})
    fs.writeFileSync(`locale/${component}.pot`, createPot(component))
}

function createPot(component) {
    const keys = {}
    const files = glob.sync(`{${componentDir(component)},${sharedDirectory()}}/elm/**/*.elm`)
    files.sort().forEach((file) => parseFile(keys, file))

    const metadata = [
        // The upload reads the language and its name from the header,
        // so the POT carries both, empty, and says what goes there: a gettext
        // editor fills in Language, and keeps X-Display-Name for the translator.
        '# Fill in the header before uploading the translation:',
        '#   Language        the language\'s code, such as de or pt-BR',
        '#   X-Display-Name  its name as people choose it, such as Deutsch',
        'msgid ""',
        'msgstr ""',
        // `<kind>:<version>`: what a translation made from this POT
        // was made for. A build that is not a release is `dev`.
        `"Project-Id-Version: ${component}-client:${getVersion() || 'dev'}\\n"`,
        '"Language: \\n"',
        '"X-Display-Name: \\n"',
        `"POT-Creation-Date: ${new Date().toISOString()} \\n"`,
        '"Content-Type: text/plain; charset=UTF-8\\n"',
        '"Content-Transfer-Encoding: 8bit\\n"',
        '"Plural-Forms: nplurals=2; plural=n == 1 ? 0 : 1;\\n"',
    ].join('\n')

    const keyLines = Object.values(keys).map((key) => keyToString(key, true)).join('\n\n')
    return metadata + '\n\n' + keyLines
}

// The shared tier whose strings are translated as part of the component: every
// app carries Common's — its components and its pages.
function sharedDirectory() {
    return 'packages/elm-common'
}

function getVersion() {
    // Version comes from the git tag pointing at HEAD (empty when not on a tag)
    try {
        return execSync('git tag --points-at HEAD', {encoding: 'utf8', stdio: ['ignore', 'pipe', 'ignore']}).split('\n')[0].trim().replace(/^v/, '')
    } catch (err) {
        return ''
    }
}


function parseFile(keys, file) {
    const moduleContent = fs.readFileSync(file, 'utf8')
    moduleContent.split('\n').forEach((line, i) => {
        let result
        while ((result = regexGettext.exec(line)) !== null) {
            const msgId = result[1]
            if (!keys[msgId]) {
                keys[msgId] = {
                    msgId,
                    usage: []
                }
            }
            keys[msgId].usage.push(`${file}:${i + 1}`)
        }

        while ((result = regexNGettext.exec(line)) !== null) {
            const msgId = result[1]
            const msgIdPlural = result[3]

            if (!keys[msgId]) {
                keys[msgId] = {
                    msgId,
                    msgIdPlural,
                    usage: []
                }
            }
            keys[msgId].usage.push(`${file}:${i + 1}`)
        }
    })
}

function keyToString(key, references) {
    const usage = references ? key.usage.map(usageToString) : []

    if (key.msgIdPlural) {
        const lines = [
            ...usage,
            `msgid "${key.msgId}"`,
            `msgid_plural "${key.msgIdPlural}"`,
            `msgstr[0] ""`,
            `msgstr[1] ""`
        ]
        return lines.join('\n')
    }

    const lines = [
        ...usage,
        `msgid "${key.msgId}"`,
        `msgstr ""`
    ]
    return lines.join('\n')
}

function usageToString(usage) {
    return `#: ${usage}`
}

module.exports = {createPot}
