# ReWitness

[![Build Status](https://travis-ci.org/bsansouci/rewitness.svg?branch=master)](https://travis-ci.org/bsansouci/rewitness)

![rewitness-gif mov](https://cloud.githubusercontent.com/assets/4534692/18104017/04c5d1aa-6eae-11e6-8922-53c6a7abf2fe.gif)

## Reason via `npm`

Example project using Reason as an `npm` dependency.

> Note: This example will be rapidly changing. It is not officially supported
> yet. Always reclone the repo each time you try it out (rebasing is not
> sufficient).

## Get Started:

To build this project you'll need [esy](https://github.com/reasonml/esy) installed globally.
You can run `npm install -g https://github.com/reasonml/esy.git\#beta-v0.0.2` to install it.

Then do the following
```sh
git clone https://github.com/bsansouci/rewitness.git
cd rewitness
esy install
esy build
npm start
```

## Making your own changes

To make your own changes, edit `src/index.re` and then run:

```
esy build && npm start
```

## Making your own changes

To make your own changes, edit `src/index.re` and then run:

```
npm run build && npm start
```

## Included Top Level

The top level `rtop` is built in to the sandbox:

```sh
# Opens `rtop` from the sandbox.
esy rtop
```

## Editor Support

All of the IDE support, including error highlighting, autocomplete, and
syntax is included inside of the sandbox.

```sh
# Opens your `$EDITOR` with all the right tools in your `$PATH`
npm run editor
```

To make your editor load the IDE support from the sandbox:

- Make sure your `$EDITOR` variable is set if not already.
  - `export EDITOR=vim`, or `export EDITOR=atom`
- Configure your `EDITOR` to load the `Reason` plugins. See the instructions
  for [Atom](http://facebook.github.io/reason/tools.html#merlin-atom) and
  [Vim](https://github.com/facebook/reason/tree/master/editorSupport/VimReason).


### What's happening
- `esy install` will download and install all your dependencies, and run the
  `postinstall` steps for all of those dependencies.
- `esy build` will simply run the build system [rebel](https://github.com/reasonml/rebel)
  to build the project.
- `npm start` will run the script located in the `start` field of the
  `scripts` section of the `package.json` file here. The `start` script simply
  runs the binary that was built in the `esy build` step.


### How to customize
- `npm` allows `scripts` to be specified in your project's `package.json`.
  These `scripts` are a named set of commands.
- You can add new named scripts in the `package.json` `scripts` field. Once
  added, you can then run them via `npm run scriptName` from within the project
  root.


### Recompiling
- To recompile this package (but not your dependencies), remove the local build
  artifacts for this package (usually just the `_build` directory) and then run
  `esy build`.


### How to turn this project into a library
(coming soon)


### Troubleshooting:
- Check to make sure everything is installed correctly. You can run `esy someShellCommand`
  to run that shell command inside the sandbox that `esy` builds.
- You can make `esy` create a Makefile and use it to drop inside a shell for any dep (and
  for the current package). Simply run `esy build-eject` and then 
  `make -f node_modules/.cache/esy/Makefile dependencyNameHere.shell` to be dropped in a shell
  with all of the right environment setup for the dependency called `dependencyNameHere`.
- If something goes wrong, try deleting the local `node_modules` directory that
  was installed, and then try reinstalling using `esy install`. You can also clear all of the caches
  with the following command `rm -rf node_modules yarn.lock ~/Library/Caches/Yarn ~/.esy _build _install`.
  This will delete all packages you've ever installed and built with `esy`. 

