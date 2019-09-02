const glob = require('glob')
const fs = require('fs')

let error = 0

const defaultIconSetPath = 'src/elm/Common/Provisioning/DefaultIconSet.elm'
const iconSetKeys = loadIconSetKeys()

const regexFaSet = /[\s+\(]faSet "([^"]*)"/g

const ignored = [
    defaultIconSetPath,
    'src/elm/Common/Html.elm'
]

glob('src/elm/**/*.elm', (err, files) => {
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

    const moduleName = toModuleName(file)
    const moduleContent = fs.readFileSync(file, 'utf8')

    testIconSet(moduleName, moduleContent, regexFaSet)
}


function toModuleName(file) {
    return file.replace('src/elm/', '').replace('.elm', '').replace(/\//g, '.')
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
