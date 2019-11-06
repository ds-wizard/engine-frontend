const glob = require('glob')
const fs = require('fs')
const utils = require('./utils')

let error = 0

const {moduleName, componentSource, defaultLocalePath} = utils.getComponentData()
const localeKeys = loadLocaleKeys()

const regexL = /\s+l "([^"]*)"/
const regexLG = /[\s+\(]lg "([^"]*)"/g
const regexLGX = /[\s+\(]lgx "([^"]*)"/g
const regexLR = /[\s+\(]lr "([^"]*)"/g
const regexLF = /\s+lf "([^"]*)"/
const regexLH = /\s+lh "([^"]*)"/
const regexLX = /\s+lx "([^"]*)"/
const regexL_ = /[\s+\(]l_ "([^"]*)"/g
const regexLF_ = /[\s+\(]lf_ "([^"]*)"/g
const regexLH_ = /[\s+\(]lh_ "([^"]*)"/g
const regexLX_ = /[\s+\(]lx_ "([^"]*)"/g

const ignored = [
    defaultLocalePath,
    `${componentSource}/${moduleName}/Common/Locale.elm`
]

glob(`${componentSource}/**/*.elm`, (err, files) => {
    files.forEach(testFile)

    Object.entries(localeKeys).forEach(([key, value]) => {
        if (value === 0) {
            reportError('LOCALE', `Key ${key} is never used`)
        }
    })

    if (error === 0) {
        console.info('OK')
    }

    process.exit(error)
})


// ---

function loadLocaleKeys() {
    const fileContent = fs.readFileSync(defaultLocalePath, 'utf8')
    const localeKeys = {}
    const regex = /\( "(.*)", ".*" \)/g

    let result
    while ((result = regex.exec(fileContent)) !== null) {
        localeKeys[result[1]] = 0
    }

    return localeKeys
}


function testFile(file) {
    if (ignored.some(ignored => ignored === file)) {
        return
    }

    const moduleName = utils.toModuleName(componentSource, file)
    const moduleContent = fs.readFileSync(file, 'utf8')

    testLocaleGlobal(moduleName, moduleContent, '_global', regexLG)
    testLocaleGlobal(moduleName, moduleContent, '_global', regexLGX)
    testLocaleGlobal(moduleName, moduleContent, '__routing', regexLR)
    testLocale(moduleName, moduleContent, regexL, regexL_)
    testLocale(moduleName, moduleContent, regexLF, regexLF_)
    testLocale(moduleName, moduleContent, regexLH, regexLH_)
    testLocale(moduleName, moduleContent, regexLX, regexLX_)
}


function testLocaleGlobal(moduleName, moduleContent, prefix, regexUse) {
    let result
    while ((result = regexUse.exec(moduleContent)) !== null) {
        testKey(moduleName, `${prefix}.${result[1]}`)
    }
}


function testLocale(moduleName, moduleContent, regexDef, regexUse) {
    const module = getModule(moduleContent, regexDef)
    if (module !== null) {
        if (module !== moduleName) {
            reportError(moduleName, `Localization module string (l) mismatch "${module}".`)
            return
        }

        let result
        while ((result = regexUse.exec(moduleContent)) !== null) {
            testKey(moduleName, `${moduleName}.${result[1]}`)
        }
    }
}


function testKey(moduleName, key) {
    if (!(key in localeKeys)) {
        reportError(moduleName, `Key "${key}" does not exist in locale.`)
    } else {
        localeKeys[key]++
    }
}


function getModule(content, regex) {
    const match = content.match(regex)
    if (match !== null) {
        return match[1]
    }
    return null
}


function reportError(module, msg) {
    console.error(`ERROR in ${module}: ${msg}`)
    error = 1
}
