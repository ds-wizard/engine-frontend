// Where each buildable component lives, for webpack, the build scripts and the
// translations. `COMPONENT=dsw` builds apps/dsw into dist/dsw.
const components = {
    dsw: {dir: 'apps/dsw', port: 8080, publicPath: '/'},
    registry: {dir: 'apps/registry', port: 8081, publicPath: '/'},
}

function component(name) {
    const found = components[name]
    if (!found) throw new Error(`Unknown component "${name}"; one of: ${Object.keys(components).join(', ')}`)
    return found
}

module.exports = {components, component}

// `node tools/build/components.js dsw` prints the component's directory, for the shell scripts.
if (require.main === module) console.log(component(process.argv[2]).dir)
