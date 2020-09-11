
all : out/test1.out

out/test1.out : l4/test1.l4 src/AbsL.hs
	stack exec l4 l4/test1.l4 > out/test1.out
	cat out/test1.out

src/AbsL.hs : l4.bnfc
	(cd src; bnfc ../l4.bnfc; rm TestL.hs)
	stack build

l4/test1.l4 l4.bnfc : ../README.org
	emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "../README.org")'

clean :
	rm out/* src/* l4/* prolog/*
