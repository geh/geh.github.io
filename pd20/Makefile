.PHONY: index
index:
	pandoc -w html -s -c practico.css index.md -o index.html --self-contained
	pandoc -w html -s -c practico.css eval1.md -o eval1.html --self-contained
	pandoc -w html -s -c practico.css proyecto.md -o proyecto.html --self-contained
	pandoc -w html -s -c practico.css ej03.md -o ej03.html --self-contained --toc --toc-depth 1 -N
	pandoc -w html -s -c practico.css ej04.md -o ej04.html --self-contained --toc --toc-depth 2 -N
	pandoc -w html -s -c practico.css ej06.md -o ej06.html --self-contained --toc
	pandoc -w html -s -c practico.css ej07.md -o ej07.html --self-contained --toc
	pandoc -w html -s -c practico.css ej08.md -o ej08.html --self-contained --toc
	pandoc -w html -s -c practico.css MonoidFunctor.md -o MonoidFunctor.html --self-contained --toc
	pandoc -w html -s -c practico.css ClasesMultiples.md -o ClasesMultiples.html --self-contained --toc
	pandoc -w html -s -c practico.css teoricoIO.md -o teoricoIO.html --self-contained --toc
	pandoc -w html -s -c practico.css ej09.md -o ej09.html --self-contained --toc
	pandoc -w html -s -c practico.css ej10.md -o ej10.html --self-contained --toc
	pandoc -w html -s -c practico.css Monad.md -o Monad.html --self-contained --toc
	pandoc -w html -s -c practico.css brick.md -o brick.html --self-contained --toc
	pandoc -w html -s -c practico.css quickcheck.md -o quickcheck.html --self-contained --toc
	pandoc -w html -s -c practico.css ej11.md -o ej11.html --self-contained --toc

.PHONY: scp
scp:
	scp index.html 0*.html proy*.html ej*.hs ej*.html  eval1.html Mon*.html Clases*.html teori*.html brick.html quickc*.html russell:public_html/pd20/

.PHONY: teorico
teorico:
	pandoc IO_monad_quickcheck.md  -o IO_monad_quickcheck.html -s --self-contained  -c practico.css --toc

.PHONY: practico
practico:
#	pandoc practico01.md  -o practico01.html -s --self-contained  -c practico.css --toc
