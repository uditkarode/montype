#!/bin/bash


RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

function check_installed() {
	if [ -z "$(which ${1})" ] && [ ! -f "/bin/${1}" ]; then
                echo "false"
        else
                echo "true"
	fi
}

echo '-------------- building release binary --------------'

if [ "$(check_installed cabal)" = "true" ]; then
    cabal new-build -O2 --disable-debug-info --enable-executable-stripping --enable-library-stripping --disable-debug-info --disable-library-for-ghci --enable-split-sections
else
    echo -e "${RED}[fatal]${NC}: ${1} executable not found, cannot proceed"; exit
fi

if [ "$(check_installed strip)" == "true" ]; then
    strip dist-newstyle/build/x86_64-linux/ghc-8.8.4/MonType-*/x/MonType/opt/build/MonType/MonType
else
    echo -e "${YELLOW}[warn]${NC}: 'strip' not found in PATH, not stripping executable"
fi

if [ "$(check_installed upx)" == "true" ]; then
    rm -f montype
    upx -9 dist-newstyle/build/x86_64-linux/ghc-8.8.4/MonType-*/x/MonType/opt/build/MonType/MonType -o montype
    echo -e "${GREEN}[success]${NC}: release binary saved to 'montype'!"
else
    echo -e "${YELLOW}[warn]${NC}: 'upx' not found in PATH, not compressing executable"
    cp dist-newstyle/build/x86_64-linux/ghc-8.8.4/MonType-*/x/MonType/opt/build/MonType/MonType montype
    echo "${GREEN}[success]${NC}: release binary saved to 'montype'!"
fi
