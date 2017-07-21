# ReWitness

[![Build Status](https://travis-ci.org/bsansouci/rewitness.svg?branch=master)](https://travis-ci.org/bsansouci/rewitness)

![rewitness-gif mov](https://cloud.githubusercontent.com/assets/4534692/18104017/04c5d1aa-6eae-11e6-8922-53c6a7abf2fe.gif)

## Reason via `npm`

Example project using Reason as an `npm` dependency.

> Note: This example will be rapidly changing. It is not officially supported
> yet. Always reclone the repo each time you try it out (rebasing is not
> sufficient).

## Get Started:

Do the following
```sh
git clone https://github.com/bsansouci/rewitness.git
cd rewitness
npm install
./node_modules/.bin/bsb -make-world
```

## Making your own changes

To make your own changes, edit `src/index.re` and then run:

```
./node_modules/.bin/bsb -make-world
```
