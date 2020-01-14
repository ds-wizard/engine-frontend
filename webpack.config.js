const path = require('path')
const CopyWebpackPlugin = require('copy-webpack-plugin')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const MiniCssExtractPlugin = require('mini-css-extract-plugin')
const OptimizeCssAssetsPlugin = require('optimize-css-assets-webpack-plugin')

const component = `engine-${process.env.COMPONENT}`

module.exports = {
    entry: [
        `./${component}/index.js`,
        `./${component}/scss/main.scss`
    ],

    output: {
        path: path.resolve(`${__dirname}/dist/${component}/`),
        filename: '[name].[chunkhash].js',
        publicPath: '/'
    },

    module: {
        rules: [
            {
                test: /\.(scss|css)$/,
                use: [{loader: MiniCssExtractPlugin.loader},
                    'css-loader',
                    'sass-loader'
                ]
            },
            {
                test: /\.html$/,
                exclude: /node_modules/,
                loader: 'file-loader?name=[name].[ext]'
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: process.env.NODE_ENV === 'production' ? 'elm-webpack-loader?verbose=true&optimize=true' : 'elm-webpack-loader?verbose=true'
            },
            {
                test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                loader: 'url-loader?limit=10000&mimetype=application/font-woff&name=[name].[ext]'
            },
            {
                test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                loader: 'file-loader?name=[name].[ext]'
            }
        ],

        noParse: /\.elm$/
    },

    plugins: [
        new HtmlWebpackPlugin({
            template: `${component}/index.ejs`
        }),
        new MiniCssExtractPlugin({
            filename: '[name].[chunkhash].css',
            allChunks: true
        }),
        new OptimizeCssAssetsPlugin({
            cssProcessorPluginOptions: {
                preset: ['default', {discardComments: {removeAll: true}}]
            }
        }),
        new CopyWebpackPlugin([
            {from: `${component}/img`, to: 'img'},
            {from: `${component}/favicon.ico`, to: 'favicon.ico'}
        ])
    ],

    devServer: {
        inline: true,
        stats: {colors: true},
        historyApiFallback: {disableDotRule: true}
    }
}
