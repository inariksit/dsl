#!/bin/sh

(cd src; bnfc l4.bnfc; rm TestL.hs)

stack build

stack exec l4 l4/test1.l4

