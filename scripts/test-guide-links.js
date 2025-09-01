const fs = require('fs')


const guideLinksFile = 'app-wizard/elm/Wizard/Utils/WizardGuideLinks.elm'

const regexDef = /\( "([^"]*)"/g
const regexUse = /get "([^"]*)"/g


fileContent = fs.readFileSync(guideLinksFile, 'utf8')

const definedKeys = []
const usedKeys = []

let result
while ((result = regexDef.exec(fileContent)) !== null) {
    definedKeys.push(result[1])
}

while ((result = regexUse.exec(fileContent)) !== null) {
    usedKeys.push(result[1])
}

const definedNotUsed = definedKeys.filter(key => !usedKeys.includes(key))
const usedNotDefined = usedKeys.filter(key => !definedKeys.includes(key))

let error = false

if (definedNotUsed.length > 0) {
    console.error('Some guide link keys are defined but not used:', definedNotUsed)
    error = true
}

if (usedNotDefined.length > 0) {
    console.error('Some guide link keys are used but not defined:', usedNotDefined)
    error = true
}

if (error) {
    process.exit(1)
}
