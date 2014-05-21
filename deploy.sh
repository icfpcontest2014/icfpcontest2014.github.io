#!/bin/sh
NOW=$(date +"%y-%m-%d %H:%M")

cd _site
git status
git add --all
git commit -m "snapshot $NOW"
git push origin master

