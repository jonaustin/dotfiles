#!/bin/bash
## for some odd reason running git submodule update causes the branch to change from master to 'no branch' wtf..
for n in *; do cd $n; echo $n; git co master; cd -; echo;done
for n in *; do cd $n; echo $n; git pull; cd -; echo;done
