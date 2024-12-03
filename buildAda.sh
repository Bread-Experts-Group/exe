#!/bin/bash
mkdir -p bin/$1;
alr exec -- gnatpp -Pexe.gpr --max-line-length=179 --wide-character-encoding=8 --eol=lf -k --layout=default -U;

set -e
alr build --profiles=*=$1 -- -o $1/exe-native -Xbinder="" -Xlinker="" -cargs -march=native;

alr exec -- gnatclean -Pexe.gpr;

alr build --profiles=*=$1 -- -o $1/exe-native-shared -Xbinder="-shared" -Xlinker="-s" -cargs "-march=native";