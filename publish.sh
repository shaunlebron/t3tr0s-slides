#!/bin/bash

set -ex

deploy_dir=deployed

if [ ! -d "$deploy_dir" ]; then
  git clone git@github.com:shaunlebron/t3tr0s-slides.git $deploy_dir
fi


# Commit and push
pushd $deploy_dir

git checkout gh-pages

rm -rf *
cp -r ../public/* .
rm -rf out-dev
rm dev.html
rm t3tr0s_slides.js
rm t3tr0s_slides.js.map


git add -u
git add .
git commit -m "manual update from publish.sh"
git push

popd
