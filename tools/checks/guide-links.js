// Every guide link a module asks for is in its application's defaults, and
// every default is asked for (tools/guide-links/guide-links-sources.js).
const {sources, readLinks, readUsedKeys} = require('../guide-links/guide-links-sources')

let error = false

for (const {name, files, users} of sources) {
    const definedKeys = Object.keys(Object.assign({}, ...files.map(readLinks)))
    const usedKeys = [...new Set(users.flatMap(readUsedKeys))]
    const definedNotUsed = definedKeys.filter(key => !usedKeys.includes(key))
    const usedNotDefined = usedKeys.filter(key => !definedKeys.includes(key))

    if (definedNotUsed.length > 0) {
        console.error(`${name}: some guide link keys are defined but not used:`, definedNotUsed)
        error = true
    }
    if (usedNotDefined.length > 0) {
        console.error(`${name}: some guide link keys are used but not defined:`, usedNotDefined)
        error = true
    }
}

if (error) {
    process.exit(1)
}
