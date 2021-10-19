#!/bin/sh
shout() { echo "$0: $*" >&2; }
barf() { shout "$*"; exit 111; }
safe() { "$@" || barf "Cannot $*"; }

safe yarn build;
safe git add .;
safe git commit -sm "pushing new gh-pages build";
safe git push origin `git subtree split --prefix build main`:gh-pages --force;

exit 0
