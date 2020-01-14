const glob = require('glob')
const fs = require('fs')
const utils = require('./utils')

let error = 0

const {componentSource, defaultIconSetPath} = utils.getComponentData()
const iconSetKeys = loadIconSetKeys()

const regexFaSet = /[\s+\(]faSet "([^"]*)"/g
const regexFaKeyClass = /[\s+\(]faKeyClass "([^"]*)"/g

const ignored = [
    defaultIconSetPath,
    `${componentSource}/Common/Html.elm`
]

glob(`${componentSource}/**/*.elm`, (err, files) => {
    files.forEach(testFile)

    Object.entries(iconSetKeys).forEach(([key, value]) => {
        if (value === 0) {
            reportError('ICON SET', `Key ${key} is never used`)
        }
    })

    if (error === 0) {
        console.info('OK')
    }

    process.exit(error)
})


// ---

function loadIconSetKeys() {
    const fileContent = fs.readFileSync(defaultIconSetPath, 'utf8')
    const iconSetKeys = {}
    const regex = /\( "(.*)", ".*" \)/g

    let result
    while ((result = regex.exec(fileContent)) !== null) {
        iconSetKeys[result[1]] = 0
    }

    return iconSetKeys
}


function testFile(file) {
    if (ignored.some(ignored => ignored === file)) {
        return
    }

    const moduleName = utils.toModuleName(componentSource, file)
    const moduleContent = fs.readFileSync(file, 'utf8')

    testIconSet(moduleName, moduleContent, regexFaSet)
    testIconSet(moduleName, moduleContent, regexFaKeyClass)
}


function testIconSet(moduleName, moduleContent, regexUse) {
    let result
    while ((result = regexUse.exec(moduleContent)) !== null) {
        testKey(moduleName, `${result[1]}`)
    }
}


function testKey(moduleName, key) {
    if (!(key in iconSetKeys)) {
        reportError(moduleName, `Key "${key}" does not exist in icon set.`)
    } else {
        iconSetKeys[key]++
    }
}


function reportError(module, msg) {
    console.error(`ERROR in ${module}: ${msg}`)
    error = 1
}
