const fs = require('fs')


const elmFile = 'app-wizard/elm/Wizard/Common/GuideLinks.elm'
const fileContent = fs.readFileSync(elmFile, 'utf8')
const keyValueRegex = /\( "(.*?)", "(.*?)" \)/g
const result = {}

let line
while ((line = keyValueRegex.exec(fileContent)) !== null) {
    result[line[1]] = line[2]
}

fs.writeFileSync('guide-links.json', JSON.stringify(result, null, 2))
