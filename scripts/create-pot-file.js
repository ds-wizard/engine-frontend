const _ = require('lodash');
const {execSync} = require('child_process')
const fs = require('fs')
const glob = require('glob')

const {component} = getComponentData()
const version = getVersion()


const regexGettext = /[\s+(]gettext "(.*?[^\\])?(\\\\)*"/g
const regexNGettext = /[\s+(]ngettext \( "(.*?[^\\])?(\\\\)*", "(.*?[^\\])?(\\\\)*" \)/g

const keys = {}

// npm install po2json@1.0.0-beta
// npx po2json -f jed cs.po cs.json

glob.glob(`{app-${component},shared/common}/elm/**/*.elm`).then((files) => {
    files.forEach(parseFile)

    const metadata = [
        'msgid ""',
        'msgstr ""',
        `"Project-Id-Version: ${component}-client:${version}\\n"`,
        `"POT-Creation-Date: ${new Date().toISOString()} \\n"`,
        '"Language: en\\n"',
        '"Content-Type: text/plain; charset=UTF-8\\n"',
        '"Content-Transfer-Encoding: 8bit\\n"',
        '"Plural-Forms: nplurals=2; plural=n == 1 ? 0 : 1;\\n"',
    ].join('\n')

    const keyLines = Object.values(keys).map(keyToString).join('\n\n')
    fs.writeFileSync(`locale/${component}.pot`, metadata + '\n\n' + keyLines)
})

function getVersion() {
    // Version comes from the git tag pointing at HEAD (empty when not on a tag)
    try {
        return execSync('git tag --points-at HEAD', {encoding: 'utf8'}).split('\n')[0].trim().replace(/^v/, '')
    } catch (err) {
        return ''
    }
}


function getComponentData() {
    const component = process.env.COMPONENT
    const moduleName = _.upperFirst(_.camelCase(component))
    const componentSource = `app-${component}/elm`

    return {
        component,
        moduleName,
        componentSource,
    }
}


function toModuleName(componentSource, file) {
    return file.replace(`${componentSource}/`, '').replace('.elm', '').replace(/\//g, '.')
}


function parseFile(file) {
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

function keyToString(key) {
    const usage = key.usage.map(usageToString)

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
