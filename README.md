# ReWitness

[![Build Status](https://travis-ci.org/bsansouci/rewitness.svg?branch=master)](https://travis-ci.org/bsansouci/rewitness)

![rewitness-gif mov](https://cloud.githubusercontent.com/assets/4534692/18104017/04c5d1aa-6eae-11e6-8922-53c6a7abf2fe.gif)

## Reason via `npm`

Graphical example project using Reason as an `npm` dependency. Build to JS, Native and Bytecode.

## Get Started:

Do the following
```sh
git clone https://github.com/bsansouci/rewitness.git
cd rewitness
npm install
./node_modules/.bin/bsb -make-world
```

This will build the JS version of this repo. You'll need safari to open this (or you'll need a server if you'd like to open it with Chrome. Use this `python -m SimpleHTTPServer 8000` to start a simple static server.).

To build the bytecode version open `bsconfig.json` and replace `"kind": "js"` with `"kind": "bytecode"` and re-run `./node_modules/.bin/bsb -make-world`. The executable is located at `lib/bs/index.byte`. This is an OCaml bytecocde program running in OCaml's VM.

You can also build to native machine code replacing `"kind": "js"` with `"kind": "native"`.

## Making your own changes

To make your own changes, edit `src/index.re` and then run:

```
./node_modules/.bin/bsb -make-world
```
