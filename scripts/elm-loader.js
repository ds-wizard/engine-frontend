'use strict'

/*
 * Minimal replacement for `elm-webpack-loader`.
 *
 * Dependency-free: uses only node builtins + the webpack loader API.
 * Supported options:
 *   pathToElm  - path to the elm binary (default: resolved from PATH)
 *   optimize   - pass --optimize
 *   debug      - pass --debug
 *   cwd        - directory holding elm.json (default: webpack context)
 */

const fs = require('node:fs')
const fsp = require('node:fs/promises')
const os = require('node:os')
const path = require('node:path')
const crypto = require('node:crypto')
const {spawn} = require('node:child_process')

// elm make is not safe to run concurrently against the same elm-stuff
let queue = Promise.resolve()
const serialize = (task) => {
    const result = queue.then(task, task)
    queue = result.catch(() => {})
    return result
}

const IMPORT_RE = /^import\s+([A-Z][A-Za-z0-9_]*(?:\.[A-Z][A-Za-z0-9_]*)*)/gm

function sourceDirectories(cwd) {
    const elmJson = JSON.parse(fs.readFileSync(path.join(cwd, 'elm.json'), 'utf8'))
    const dirs = elmJson['source-directories'] || ['src']
    return dirs.map((dir) => path.resolve(cwd, dir))
}

// Walks `import` statements to collect every local .elm file the entry depends on.
async function findDependencies(entry, sourceDirs) {
    const seen = new Set()
    const pending = [entry]

    while (pending.length > 0) {
        const file = pending.pop()
        if (seen.has(file)) continue
        seen.add(file)

        let contents
        try {
            contents = await fsp.readFile(file, 'utf8')
        } catch (err) {
            continue
        }

        for (const match of contents.matchAll(IMPORT_RE)) {
            const relative = match[1].split('.').join(path.sep) + '.elm'
            for (const dir of sourceDirs) {
                const candidate = path.join(dir, relative)
                if (!seen.has(candidate) && fs.existsSync(candidate)) {
                    pending.push(candidate)
                    break
                }
            }
        }
    }

    seen.delete(entry)
    return [...seen]
}

function runElm(elmBinary, args, cwd) {
    return new Promise((resolve, reject) => {
        // 'inherit' lets the Elm compiler see a TTY, so its error reports keep their colours
        const child = spawn(elmBinary, args, {cwd, stdio: 'inherit'})
        child.on('error', reject)
        child.on('close', (code) => {
            if (code === 0) {
                resolve()
            } else {
                reject(new Error('Elm compilation failed (see the output above).'))
            }
        })
    })
}

async function compile(entry, cwd, options) {
    const outputDir = await fsp.mkdtemp(path.join(os.tmpdir(), 'elm-loader-'))
    const output = path.join(outputDir, 'elm-' + crypto.randomBytes(6).toString('hex') + '.js')

    const args = ['make', path.relative(cwd, entry), '--output=' + output]
    if (options.optimize) args.push('--optimize')
    if (options.debug) args.push('--debug')

    try {
        await runElm(options.pathToElm || 'elm', args, cwd)
        return await fsp.readFile(output, 'utf8')
    } finally {
        await fsp.rm(outputDir, {recursive: true, force: true})
    }
}

module.exports = function elmLoader() {
    const callback = this.async()
    const options = this.getOptions() || {}
    const entry = this.resourcePath
    const cwd = options.cwd ? path.resolve(options.cwd) : this.rootContext

    this.cacheable && this.cacheable()

    const isProduction = this._compiler.options.mode === 'production'
    const resolved = {
        pathToElm: options.pathToElm ? path.resolve(cwd, options.pathToElm) : undefined,
        optimize: options.optimize !== undefined ? options.optimize : isProduction,
        debug: options.debug !== undefined ? options.debug : !isProduction,
        cwd
    }

    const sourceDirs = sourceDirectories(cwd)

    // Only worth walking the import graph when webpack is watching for changes.
    const dependencies = this._compiler.watching
        ? findDependencies(entry, sourceDirs).then((files) => {
            this.addDependency(path.join(cwd, 'elm.json'))
            files.forEach((file) => this.addDependency(file))
        }).catch((err) => {
            this.emitWarning(err)
        })
        : Promise.resolve()

    dependencies
        .then(() => serialize(() => compile(entry, cwd, resolved)))
        .then((code) => callback(null, code), (err) => callback(err))
}
