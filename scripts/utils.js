const _ = require('lodash')


function getComponentData() {
    const component = process.env.COMPONENT
    const moduleName = _.upperFirst(_.camelCase(component))
    const componentSource = `app-${component}/elm`

    return {
        component,
        moduleName,
        componentSource,
    }
}


function toModuleName(componentSource, file) {
    return file.replace(`${componentSource}/`, '').replace('.elm', '').replace(/\//g, '.')
}


module.exports = {
    getComponentData,
    toModuleName
}
