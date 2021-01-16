#!/usr/bin/bash env

echo Use updated \'rapid\' pagkage
cabal2nix https://github.com/eyeinsky/rapid.git > github-eyeinsky-rapid.nix
