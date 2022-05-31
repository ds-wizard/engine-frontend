const path = require('path')
const CopyWebpackPlugin = require('copy-webpack-plugin')
const CssMinimizerPlugin = require('css-minimizer-webpack-plugin')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const MiniCssExtractPlugin = require('mini-css-extract-plugin')
const TerserPlugin = require("terser-webpack-plugin")

const component = `engine-${process.env.COMPONENT}`

const components = {
    'engine-wizard': {
        port: 8080,
        extraEntries: [
            './node_modules/chart.js/dist/chart.js'
        ]
    },
    'engine-registry': {
        port: 8081,
        extraEntries: []
    }
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
        publicPath: '/'
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
                                quietDeps: true
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
                {from: `${component}/img`, to: 'img'},
                {from: `${component}/favicon.ico`, to: 'favicon.ico'}
            ]
        })
    ],

    devServer: {
        historyApiFallback: {disableDotRule: true},
        port: components[component].port,
        static: {
            directory: __dirname
        }
    }
}
