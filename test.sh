#!/usr/bin/env bash

ban SBCL
sbcl --load test/run.lisp --disable-debugger --quit
