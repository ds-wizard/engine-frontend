const fs = require('fs')
const csv = require('fast-csv')
const utils = require('./utils')


const {defaultLocalePath, component} = utils.getComponentData()

let localeContent = fs.readFileSync(defaultLocalePath, 'utf8')

fs.createReadStream(`locale/${component}.csv`)
    .pipe(csv.parse({headers: true}))
    .on('data', (row) => {
        const regexp = new RegExp(`\\( "${row.key}", ".*" \\)`, 'g')
        const replacement = `( "${row.key}", "${row.value}" )`
        localeContent = localeContent.replace(regexp, replacement)
    })
    .on('end', (count) => {
        fs.writeFileSync(defaultLocalePath, localeContent)
    })
