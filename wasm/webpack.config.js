'use strict';

const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const BrotliWebpackPlugin = require('brotli-webpack-plugin');
const CompressionWebpackPlugin = require('compression-webpack-plugin');
const TerserPlugin = require("terser-webpack-plugin");
const CssMinimizerPlugin = require("css-minimizer-webpack-plugin");
const { GitRevisionPlugin } = require('git-revision-webpack-plugin')
const gitRevisionPlugin = new GitRevisionPlugin()
const webpack = require('webpack');
const isWebpackDevServer = process.argv.some(a => path.basename(a) === 'webpack-dev-server');
const isWatch = process.argv.some(a => a === '--watch');
const isHot = process.argv.some(a => a === '--hot');
const isDev = isWebpackDevServer || isWatch || process.env.NODE_ENV !== 'production';

let plugins =
  isWebpackDevServer || !isWatch ? [] : [
    function(){
      this.plugin('done', function(stats){
        process.stderr.write(stats.toString('errors-only'));
      });
    }
  ]
;

if (!isDev) {
  plugins.push(new BrotliWebpackPlugin({
    asset: '[path].br[query]',
    test: /\.(js|css|html|svg)$/,
    threshold: 10240,
    minRatio: 0.8
  }));
  plugins.push(new CompressionWebpackPlugin({
    filename: '[file].gz[query]',
    algorithm: 'gzip',
    test: /\.(js|css|html|svg)$/,
    threshold: 10240,
    minRatio: 0.8
  }));
}

module.exports = {
  devtool: 'eval-source-map',
  mode: isDev ? "development" : "production",
  devServer: {
    host: '0.0.0.0',
    port: 4008,
    client: {
      overlay: {
        runtimeErrors: (error) => {
          // This error pops up in the canvas and doesn't really mean anything, so we can safely ignore it.
          if (error.message === 'ResizeObserver loop limit exceeded' || error.message === 'ResizeObserver loop completed with undelivered notifications.') {
            return false;
          }

          return true;
        },
      },
    },
    static: [{
        directory: path.resolve(__dirname, 'dist'),
        watch: true,
      }
    ],
    devMiddleware: {
      stats: 'errors-only',
    },
    allowedHosts: ["all"],
    // enable hot-module-reloading for performance++
    hot: isHot,
    // performance++; but restart WDS after an npm install.
    watchFiles: {
      paths: ["src/**/*"],
      options: {
        ignored: /node_modules/,
      }
    }
  },

  entry: './src/index.js',

  output: {
    path: path.resolve(__dirname, './dist'),
    filename: '[name].[contenthash].js',
  },

  module: {
    rules: [
      {
        test: /\.(png|jpg|gif)$/i,
        use: [
          {
            loader: 'url-loader',
            options: {
              limit: 8192,
            },
          },
        ],
      },
      {
        test: /\.(css)$/,
        use: [
          // MiniCssExtractPlugin.loader,
          'style-loader',
          'css-loader'
        ],
      },
      {
        test:/\.(purs)$/i,
        exclude: /node_modules/,
        use: [
          {
            loader: 'purs-loader',
            options: {
              warnings: false,
              watch: isWatch || isWebpackDevServer,
              spago: true,
              spagoDhall: "spago.dhall",
              src: [ 
                /* 
                  supplied by `spago: true`, 
                  but if not present, will cause a weird warning...
                */
              ],
            }
          }
        ]
      }
    ]
  },
  resolveLoader: {
    modules: [
      path.join(__dirname, 'node_modules')
    ]
  },
  resolve: {
    alias: {
      "~": path.resolve(__dirname, "node_modules"),
    },
    modules: [ 'node_modules' ],
    extensions: [ '.purs', '.js'],
  },
  optimization: {
    splitChunks: {
      cacheGroups: {
        commons: {
          test: /[\\/]node_modules[\\/]/,
          name: 'vendors',
          chunks: 'all'
        },
        styles: {
          name: 'styles',
          test: /\.(sa|sc|c)ss$/,
          chunks: 'all',
          enforce: true,
        }
      }
    },
    minimize: !isDev,
    minimizer: [
      new TerserPlugin(),
      new CssMinimizerPlugin({
        minimizerOptions: {
          preset: [
            'default',
            {
              discardComments: {
                removeAll: true
              }
            },
          ],
        },
      }),
      '...'
    ],
  },
  plugins: [
    // #justjavascriptthingz:
    // react-map-gl checks for `typeof process !== 'undefined'` which means
    // that even though DefinePlugin below correctly substitutes 'process.env.MapboxAccessToken',
    // the branch is never taken, and you end up with no mapbox API token loaded.
    //
    // Note that `process/browser` just provides an empty `process.env = {}` -- so we still
    // depend on the `DefinePlugin` substituting `process.env.XXX` to work...
    new webpack.ProvidePlugin({
      process: 'process/browser.js'
    }),
    // Work around for Buffer is undefined:
    // https://github.com/webpack/changelog-v5/issues/10
    new webpack.ProvidePlugin({
      Buffer: ['buffer', 'Buffer'],
    }),
    new webpack.DefinePlugin({
      'process.env.SERVER_URL': JSON.stringify(process.env.SERVER_URL),
      'process.env.FRONTEND_GIT': JSON.stringify(gitRevisionPlugin.branch()+'@'+gitRevisionPlugin.commithash()),
    }),
    new webpack.LoaderOptionsPlugin({
      debug: true
    }),
    new HtmlWebpackPlugin({
      title: 'FOAM Hostel',
      template: 'index.html',
      inject: true  // See stackoverflow.com/a/38292765/3067181
    }),
    function(compiler) {
      compiler.hooks.done.tap("purs-loader errors", function(stats){
        process.stderr.write(stats.toString('errors-only'));
      })
    },
  ].concat(plugins)
};