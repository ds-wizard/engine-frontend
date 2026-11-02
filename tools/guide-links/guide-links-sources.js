// Where each application's default guide links are defined, and which modules
// ask for them, for `tools/checks/guide-links.js`.
const fs = require('fs')

const sources = [
    {
        name: 'dsw',
        files: ['apps/dsw/elm/Dsw/Data/GuideLinks.elm'],
        users: ['packages/elm-common/elm/Common/Data/GuideLinks.elm'],
    },
]

function readLinks(file) {
    const content = fs.readFileSync(file, 'utf8')
    const regex = /\( "([^"]*)", "([^"]*)" \)/g
    const links = {}
    let match
    while ((match = regex.exec(content)) !== null) links[match[1]] = match[2]
    return links
}

function readUsedKeys(file) {
    const content = fs.readFileSync(file, 'utf8')
    const regex = /get "([^"]*)"/g
    const keys = []
    let match
    while ((match = regex.exec(content)) !== null) keys.push(match[1])
    return keys
}

module.exports = {sources, readLinks, readUsedKeys}
