const path = require('path')
const webpack = require('webpack')
const CopyWebpackPlugin = require('copy-webpack-plugin')
const CssMinimizerPlugin = require('css-minimizer-webpack-plugin')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const MiniCssExtractPlugin = require('mini-css-extract-plugin')
const TerserPlugin = require("terser-webpack-plugin")

const {component: findComponent} = require('./tools/build/components')
const PotPlugin = require('./tools/build/pot-plugin')

const name = process.env.COMPONENT
const component = findComponent(name)
const dir = component.dir


module.exports = {
    mode: process.env.NODE_ENV === 'production' ? 'production' : 'development',

    entry: [
        `./${dir}/index.js`,
        `./${dir}/scss/main.scss`,
    ],

    output: {
        path: path.resolve(`${__dirname}/dist/${name}/`),
        filename: '[name].[chunkhash].js',
        publicPath: component.publicPath
    },

    module: {
        rules: [
            {
                test: /\.(scss|css)$/,
                use: [
                    MiniCssExtractPlugin.loader,
                    {
                        loader: 'css-loader',
                        options: {
                            // A url() that starts with / is the application's own
                            // public path — the logo, an illustration — and names a
                            // file CopyPlugin puts there as it is. Resolving it here
                            // would look for it on disk and emit a hashed copy, which
                            // is the opposite of what a fixed, replaceable path is for.
                            url: {filter: (url) => !url.startsWith('/')}
                        }
                    },
                    {
                        loader: 'sass-loader',
                        options: {
                            sassOptions: {
                                quietDeps: true,
                                style: 'expanded'
                            }
                        }
                    }
                ]
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: require.resolve('./tools/build/elm-loader.js'),
                options: {
                    // ELM overrides it, for a node_modules shared with another platform.
                    pathToElm: process.env.ELM || 'node_modules/.bin/elm',
                    optimize: process.env.NODE_ENV === 'production'
                }
            },
            {
                test: /\.(svg|eot|woff|woff2|ttf)$/,
                type: 'asset/resource',
                generator: {
                    filename: '[name][ext]'
                }
            },
        ],

        noParse: /\.elm$/
    },

    resolve: {
        // Node modules the PO parser (po2json → gettext-parser) asks for in the
        // browser: buffer (safe-buffer), string_decoder (iconv-lite) and util,
        // each a declared dependency, so they do not depend on what else installs them.
        fallback: {
            "stream": false,
            buffer: require.resolve("buffer/"),
            string_decoder: require.resolve("string_decoder/"),
            util: require.resolve("util/"),
            "fs": false,
            "process": false
        }
    },

    optimization: {
        splitChunks: {
            chunks: 'all'
        },
        minimize: process.env.NODE_ENV === 'production',
        minimizer: [
            new CssMinimizerPlugin({
                minimizerOptions: {
                    preset: [
                        'default',
                        {discardComments: {removeAll: true}}
                    ]
                }
            }),
            new TerserPlugin({
                extractComments: false
            }),
            '...'
        ]
    },

    performance: {
        hints: false
    },

    plugins: [
        new HtmlWebpackPlugin({
            template: `${dir}/index.ejs`,
            scriptLoading: 'module',
            // The dsw and registry apps place the bundle's tags themselves, at the end of the body:
            // a stylesheet in the head blocks the first paint, so the loading
            // spinner written into the page cannot be seen until the whole
            // stylesheet has arrived — which is exactly the wait it is there to
            // cover. Everything before an in-body stylesheet paints without
            // waiting for it. The other apps keep the injected head tags.
            inject: !['dsw', 'registry'].includes(name),
            // `minimizeOptions` was not an option the plugin has, so the
            // production HTML was minified only by the plugin's own defaults
            // and the inline style and pre-paint script were shipped as
            // written. Spelled out here, and only for a production build —
            // the development server keeps the page readable.
            minify: process.env.NODE_ENV === 'production' && {
                collapseWhitespace: true,
                keepClosingSlash: true,
                removeComments: true,
                removeRedundantAttributes: true,
                removeScriptTypeAttributes: true,
                removeStyleLinkTypeAttributes: true,
                useShortDoctype: true,
                minifyCSS: true,
                minifyJS: true
            }
        }),
        new MiniCssExtractPlugin({
            filename: '[name].[chunkhash].css'
        }),
        new CopyWebpackPlugin({
            patterns: [
                {from: `${dir}/assets`, to: 'assets', noErrorOnMissing: true},
                {from: `${dir}/favicon.ico`, to: 'favicon.ico', noErrorOnMissing: true}
            ]
        }),
        new webpack.DefinePlugin({
            'process.env.NODE_DEBUG': JSON.stringify(process.env.NODE_DEBUG),
        }),
        ...(name === 'dsw' ? [new PotPlugin(name)] : []),
    ],

    devServer: {
        historyApiFallback: {
            disableDotRule: true,
            index: component.publicPath
        },
        port: component.port,
        static: {
            directory: (name === 'registry') ? __dirname : path.join(__dirname, dir, 'static')
        }
    }
}
