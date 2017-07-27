#!/bin/bash

HIGHLIGHT='\033[1;32m'
HIGHLIGHT_END='\033[0m'

stack --stack-yaml stack.yaml.ghcjs setup
stack --stack-yaml stack.yaml.ghcjs build
OUTPUT=$(stack --stack-yaml stack.yaml.ghcjs path --local-install-root)/bin/calculator-gui.jsexe
cp style/style.css "$OUTPUT"/
printf "${HIGHLIGHT}Open the following file with your web browser to view the graphical user interface:${HIGHLIGHT_END}\n"
echo "$OUTPUT"/"index.html"

