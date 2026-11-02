// Emits the POT of the user interface as `ui.pot` beside the application, in a
// production build and on the development server alike: Settings →
// Locales downloads it. It is made again on every compilation, so the strings
// of an edited module are in it as soon as the module is.
const {createPot} = require('../i18n/create-pot-file')

class PotPlugin {
    constructor(component) {
        this.component = component
    }

    apply(compiler) {
        const {Compilation, sources} = compiler.webpack
        compiler.hooks.thisCompilation.tap('PotPlugin', (compilation) => {
            compilation.hooks.processAssets.tap(
                {name: 'PotPlugin', stage: Compilation.PROCESS_ASSETS_STAGE_ADDITIONAL},
                () => compilation.emitAsset('ui.pot', new sources.RawSource(createPot(this.component)))
            )
        })
    }
}

module.exports = PotPlugin
