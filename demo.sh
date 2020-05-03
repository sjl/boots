#!/usr/bin/env bash

sbcl-raw --load "examples/$1.lisp" --eval "(boots/examples/$1:run)" --quit
