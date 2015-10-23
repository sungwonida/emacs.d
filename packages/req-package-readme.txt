Table of Contents
─────────────────

1 req-package
.. 1.1 Description
.. 1.2 Usage
.. 1.3 El Get
.. 1.4 More?
.. 1.5 Migrate from use-package
.. 1.6 Note
.. 1.7 Logging
.. 1.8 Contribute
.. 1.9 Things to be done
..... 1.9.1 TODO take package dependencies from it's meta data
..... 1.9.2 TODO el-get/elpa packages must be in priority over builtin ones
.. 1.10 Changelog
..... 1.10.1 v1.0
..... 1.10.2 v0.9
..... 1.10.3 v0.8
..... 1.10.4 v0.7
..... 1.10.5 v0.6
..... 1.10.6 v0.5
..... 1.10.7 v0.4.2
..... 1.10.8 v0.4.1
..... 1.10.9 v0.4-all-cycles
..... 1.10.10 v0.3-cycles
..... 1.10.11 v0.2-auto-fetch


1 req-package
═════════════

  [[file:https://img.shields.io/badge/license-GPL_3-green.svg]]
  [[file:http://melpa.org/packages/req-package-badge.svg]]
  [[file:http://stable.melpa.org/packages/req-package-badge.svg]]
  [[file:https://travis-ci.org/edvorg/req-package.svg]]
  [[file:https://coveralls.io/repos/edvorg/req-package/badge.svg?branch=develop&service=github]]


  [[file:https://img.shields.io/badge/license-GPL_3-green.svg]]
  http://www.gnu.org/licenses/gpl-3.0.txt

  [[file:http://melpa.org/packages/req-package-badge.svg]]
  http://melpa.org/#/req-package

  [[file:http://stable.melpa.org/packages/req-package-badge.svg]]
  http://stable.melpa.org/#/req-package

  [[file:https://travis-ci.org/edvorg/req-package.svg]]
  https://travis-ci.org/edvorg/req-package

  [[file:https://coveralls.io/repos/edvorg/req-package/badge.svg?branch=develop&service=github]]
  https://coveralls.io/github/edvorg/req-package?branch=develop


1.1 Description
───────────────

  req-package solves one single problem - make order of package
  configurations in your init.el right without continuous reordering
  your code while still providing ambrosian [use-package] goodness.  It
  makes your .emacs.d code more strict and modular, and less error
  prone.  You can look here, how I divided my code in separate modules
  and how simple it looks
  [https://github.com/edvorg/emacs-configs/tree/master/init.d] .

  Remember, how often you tackled into problem, when you need to require
  one package, do some configuration, then the same with second and so
  on. Sometimes it becomes too complex.  Especially in cases when one
  package have more than one dependency.  You can draw a graph of
  dependencies in your configuration, and, I'm sure, it's complex.
  req-package creates this graph for you and makes a correct traverse on
  it.  The syntax is almost the same as with use-package, but it
  provides a few additional keywords:
  1) :require - a parameter to specify dependencies
  2) :loader - an optional parameter to specify where to get package
     (el-get, elpa, etc.)

  Interesting thing is that packages are installed automatically once
  req-package-finish function is executed.  So there is no need for
  things like cask or save-packages.  You just write a configuration
  with packages you need and they will be there.  req-package will try
  to use elpa, el-get or any package system provided by you to find and
  install your packages.


  [use-package] https://github.com/jwiegley/use-package


1.2 Usage
─────────

  Load req-package:

  ┌────
  │ (require 'req-package)
  └────

  Define required packages with dependencies using `:require' like this:

  ┌────
  │ (req-package dired) ;; you can omit this empty requirement because of dired-single
  │
  │ (req-package dired-single
  │   :require dired
  │   :config (...))
  │
  │ (req-package lua-mode
  │   :config (...))
  │
  │ (req-package flymake)
  │
  │ (req-package flymake-lua
  │   :require flymake lua-mode
  │   :config (...))
  └────

  To start loading packages in right order:

  ┌────
  │ (req-package-finish)
  └────


1.3 El Get
──────────

  There is another benefit over use-package - `el-get' support.  No more
  thinking about sources for your packages.  Just install and configure
  your el-get.  Here is example:

  ┌────
  │ (require 'req-package)
  │
  │ (req-package-force el-get
  │   :init
  │   (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes")
  │   (el-get 'sync))
  │
  │ (req-package gotham-theme
  │   :config
  │   (print "gotham theme is here and installed from el-get"))
  │
  │ (req-package-finish)
  └────

  Also, of course, there could be dependencies between el-get and elpa
  packages


1.4 More?
─────────

  You can always extend list of package providers or change priorities
  if you want.  in which your packages are being installed.  It can be
  done by customizing `req-package-providers' map.  It's a mapping
  loader-symbol -> (list install-function package-present-p-function)


1.5 Migrate from use-package
────────────────────────────

  Just replace all `(use-package ...)' with `(req-package [:require
  DEPS] ...)' and add `(req-package-finish)' at the end of your
  configuration file.


1.6 Note
────────

  All use-package parameters are supported, see use-package manual.  for
  additional info.

  However, there is no need for the `:ensure' keyword; req-package will
  add it automatically if needed.

  For each package you can manually specify loader function by `:loader'
  keyword.  It can be any key for `req-package-providers' map.

  Also there is a `req-package-force' function which simulates plain old
  use-package behavior.

  More complex req-package usage example can be found at
  [http://github.com/edvorg/emacs-configs].


1.7 Logging
───────────

  You can use `req-package--log-open-log' to see, what is happening with
  your configuration.  You can choose log level in `req-package' group
  by `req-package-log-level' custom.  These log levels are supported:
  `fatal', `error', `warn', `info', `debug', `trace'.


1.8 Contribute
──────────────

  Please, commit and pull-request your changes to `develop' branch.
  Master is used for automatic repo package builds by melpa's travis-ci.


1.9 Things to be done
─────────────────────

1.9.1 TODO take package dependencies from it's meta data
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌


1.9.2 TODO el-get/elpa packages must be in priority over builtin ones
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌


1.10 Changelog
──────────────

1.10.1 v1.0
╌╌╌╌╌╌╌╌╌╌╌

  • proper errors handling. see `req-package–log-open-log` for messages
  • smart add-hook which invokes function if mode is loaded
  • refactor providers system
  • no need to use progn in :init and :config sections
  • no need to use list literal in :require section


1.10.2 v0.9
╌╌╌╌╌╌╌╌╌╌╌

  • `:loader' keyword support


1.10.3 v0.8
╌╌╌╌╌╌╌╌╌╌╌

  • bugfixes


1.10.4 v0.7
╌╌╌╌╌╌╌╌╌╌╌

  • fixed some issues with packages installation. all packages will be
    installed at bootstrap time
  • custom package providers support by `req-package-providers'
  • priority feature for cross provider packages loading. you can
    choose, what to try first - elpa, el-get, or something else


1.10.5 v0.6
╌╌╌╌╌╌╌╌╌╌╌

  • `el-get' support


1.10.6 v0.5
╌╌╌╌╌╌╌╌╌╌╌

  • Major system refactoring.
  • Fixed bugs with defered loading.
  • Significant performance optimization.
  • `max-specpdl-size', `max-lisp-eval-depth' issues completely solved.
  • Flexible `:require' keyword parsing.


1.10.7 v0.4.2
╌╌╌╌╌╌╌╌╌╌╌╌╌

  • Bug fixes.


1.10.8 v0.4.1
╌╌╌╌╌╌╌╌╌╌╌╌╌

  • Various tweaks and bug fixes.


1.10.9 v0.4-all-cycles
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  • All cycles of your dependencies will be printed now.
  • Also there are more handy log messages and some bug fixes.


1.10.10 v0.3-cycles
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  • There are nice error messages about cycled dependencies now.
  • Cycles printed in a way: `pkg1 -> [pkg2 -> ...] pkg1'.
  • It means there is a cycle around `pkg1'.


1.10.11 v0.2-auto-fetch
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  • There is no need of explicit `:ensure' in your code now.
  • When you req-package it adds `:ensure' if package is available in
    your repos.
  • Also package deps `:ensure''d automatically too.
  • Just write `(req-package pkg1 :require pkg2)' and all you need will
    be installed.
