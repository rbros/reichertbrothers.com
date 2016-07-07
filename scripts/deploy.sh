#!/bin/bash

set -o errexit -o nounset

# Don't deploy if this is not on master
if [ "$TRAVIS_BRANCH" != "master" ]
then
  echo "This commit was against the $TRAVIS_BRANCH and not the master! No deploy!"
  exit 0
fi

DEPLOY_ID=$(curl -XPOST "https://$GH_TOKEN@api.github.com/repos/$TRAVIS_REPO_SLUG/deployments" -H "Content-Type:application/json" --data '{"ref":"master"}' | python2 -c "import json,sys;obj=json.load(sys.stdin);print obj['id'];")

rev=$(git rev-parse --short HEAD)

# Make a directory where we can build the files We will CD into this
# directory, create a git repo, copy files from parent directory, and
# then push this repo to reichertbrothers.com gh-pages branch.
mkdir stage
cd stage

# Set git settings
git init
git config user.name "Cody Reichert"
git config user.email "cody@reichertbrothers.com"

# Check out gh-pages branch of reichertbrothers.com repo
# Note: GH_TOKEN was entered into the Travis-CI environment manually
# through the UI.
git remote add upstream "https://$GH_TOKEN@github.com/rbros/reichertbrothers.com.git"
git fetch upstream
git reset upstream/gh-pages

# touch .
cp -R $TRAVIS_BUILD_DIR/_site/* .

git add -A .
git commit -m "rebuild pages at ${rev}"
git push -q upstream HEAD:gh-pages

# Update the deploy as success
DEPLOYED=$(curl -XPOST "https://$GH_TOKEN@api.github.com/repos/$TRAVIS_REPO_SLUG/deployments/$DEPLOY_ID/statuses" --data '{"state":"success"}' | python2 -c "import json,sys;obj=json.load(sys.stdin);print obj['state'];")
echo $DEPLOYED
