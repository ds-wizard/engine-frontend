const fs = require('fs')
const csv = require('fast-csv')
const utils = require('./utils')

const {defaultLocalePath, component} = utils.getComponentData()


const csvStream = csv.format({ headers: true })
csvStream
    .pipe(fs.createWriteStream(`locale/${component}.csv`))
    .on('end', () => process.exit())


const localeContent = fs.readFileSync(defaultLocalePath, 'utf8')
const regex = /\( "(.*)", "(.*)" \)/g

let result
while ((result = regex.exec(localeContent)) !== null) {
    csvStream.write({
        key: result[1],
        value: result[2]
    })
}
