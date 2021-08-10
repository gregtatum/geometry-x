#!/usr/bin/env bash

# DIPLOMAT="diplomat-tool"
DIPLOMAT="../diplomat/target/debug/diplomat-tool"

# $DIPLOMAT js js/ --docs js/docs/
$DIPLOMAT cpp bindings/cpp
