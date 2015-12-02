cd language-ecmascript-testgen
cabal sandbox init
cabal update
cabal sandbox add-source ../mozilla-js-parser-api/
cabal sandbox add-source ../../
cabal install
cd ..
ln -s testgen language-ecmascript-test/dist/build/language-ecmascript-testgen/language-ecmascript-testgen
