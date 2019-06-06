#!/bin/sh

erl -noshell -pa ./ -s client main 1 2 -s init stop
