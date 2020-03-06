const _ = require('lodash')


function getComponentData() {
    const component = process.env.COMPONENT
    const moduleName = _.upperFirst(_.camelCase(component))
    const componentSource = `engine-${component}/elm`
    const defaultLocalePath = `${componentSource}/${moduleName}/Common/Provisioning/DefaultLocale.elm`
    const defaultIconSetPath = `${componentSource}/${moduleName}/Common/Provisioning/DefaultIconSet.elm`

    return {
        component,
        moduleName,
        componentSource,
        defaultLocalePath,
        defaultIconSetPath
    }
}


function toModuleName(componentSource, file) {
    return file.replace(`${componentSource}/`, '').replace('.elm', '').replace(/\//g, '.')
}


module.exports = {
    getComponentData,
    toModuleName
}
