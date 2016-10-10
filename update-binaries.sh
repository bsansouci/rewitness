#!/bin/bash

if [ "$TRAVIS_PULL_REQUEST" == "false" ]; then
  echo -e "Starting to update osx-build\n"

  #copy data we're interested in to other place
  cp _build/src/index.native $HOME/rewitness.native

  #go to home and setup git
  cd $HOME
  git config --global user.email "travis@travis-ci.org"
  git config --global user.name "Travis"

  #using token clone osx-build branch
  git clone --quiet --branch=osx-build https://${GH_TOKEN}@github.com/bsansouci/rewitness.git  osx-build > /dev/null

  #go into diractory and copy data we're interested in to that directory
  cd osx-build
  cp $HOME/rewitness.native rewitness.native

  #add, commit and push files
  git add -f .
  git commit -m "Travis build $TRAVIS_BUILD_NUMBER pushed to osx-build"
  git push -fq origin osx-build > /dev/null

  echo -e "Done magic with coverage\n"
fi
