const glob = require('glob')
const fs = require('fs')
const utils = require('./utils')

const {component} = utils.getComponentData()


const regexGettext = /[\s+(]gettext "(.*?[^\\])?(\\\\)*"/g
const regexNGettext = /[\s+(]ngettext \( "(.*?[^\\])?(\\\\)*", "(.*?[^\\])?(\\\\)*" \)/g

const keys = {}

// npx po2json -f jed cs.po cs.json

glob(`{engine-${component},engine-shared}/elm/**/*.elm`, (err, files) => {
    files.forEach(parseFile)

    const keyLines = Object.values(keys).map(keyToString).join('\n\n')
    fs.writeFileSync(`locale/${component}.pot`, keyLines)
})


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
