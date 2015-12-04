cd language-ecmascript-testgen
cabal sandbox init
cabal update
cabal sandbox add-source ../mozilla-js-parser-api/
cabal sandbox add-source ../../
cabal install
cabal configure
cabal build
cd ..
cp language-ecmascript-testgen/dist/build/language-ecmascript-testgen/language-ecmascript-testgen ./testgen
