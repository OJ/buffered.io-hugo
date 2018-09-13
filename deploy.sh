#!/bin/bash
hugo
cd public
git add -A
git commit -m "Publishing to blog"
git push --force origin master
