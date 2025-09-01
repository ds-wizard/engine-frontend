const path = require('path')
const webpack = require('webpack')
const CopyWebpackPlugin = require('copy-webpack-plugin')
const CssMinimizerPlugin = require('css-minimizer-webpack-plugin')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const MiniCssExtractPlugin = require('mini-css-extract-plugin')
const TerserPlugin = require("terser-webpack-plugin")

const component = `app-${process.env.COMPONENT}`

const components = {
    'app-wizard': {
        port: 8080,
        extraEntries: [],
        publicPath: '/wizard/'
    },
    'app-registry': {
        port: 8081,
        extraEntries: [],
        publicPath: '/'
    },
}


module.exports = {
    mode: process.env.NODE_ENV === 'production' ? 'production' : 'development',

    entry: [
        `./${component}/index.js`,
        `./${component}/scss/main.scss`,
    ].concat(components[component].extraEntries),

    output: {
        path: path.resolve(`${__dirname}/dist/${component}/`),
        filename: '[name].[chunkhash].js',
        publicPath: components[component].publicPath
    },

    module: {
        rules: [
            {
                test: /\.(scss|css)$/,
                use: [
                    MiniCssExtractPlugin.loader,
                    'css-loader',
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
                test: /\.html$/,
                exclude: /node_modules/,
                loader: 'file-loader',
                options: {
                    name: '[name].[ext]'
                }
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: 'elm-webpack-loader',
                options: process.env.NODE_ENV === 'production' ? {
                    verbose: true,
                    optimize: true,
                    pathToElm: 'node_modules/.bin/elm'
                } : {
                    verbose: true
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
        fallback: {
            "stream": false,
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
            template: `${component}/index.ejs`,
            scriptLoading: 'blocking',
            minimizeOptions: {
                minifyCss: true
            }
        }),
        new MiniCssExtractPlugin({
            filename: '[name].[chunkhash].css'
        }),
        new CopyWebpackPlugin({
            patterns: [
                {from: `${component}/assets`, to: 'assets'},
                {from: `${component}/robots.txt`, to: 'robots.txt', noErrorOnMissing: true}
            ]
        }),
        new webpack.DefinePlugin({
            'process.env.NODE_DEBUG': JSON.stringify(process.env.NODE_DEBUG),
        }),
    ],

    devServer: {
        historyApiFallback: {
            disableDotRule: true,
            index: components[component].publicPath
        },
        port: components[component].port,
        static: {
            directory: (component === 'app-registry') ? __dirname : path.join(__dirname, component, 'static')
        }
    }
}
