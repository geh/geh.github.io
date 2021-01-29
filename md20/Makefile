.PHONY: index
index:
	pandoc -w html -s -c practico.css index.md -o index.html --self-contained
	pandoc --slide-level 2  -t revealjs -s -o 01.html 01.md   -V revealjs-url=./reveal.js
	pandoc --slide-level 2  -t revealjs -s -o 02.html 02.md   -V revealjs-url=./reveal.js
	pandoc --slide-level 2  -t revealjs -s -o 03.html 03.md   -V revealjs-url=./reveal.js
	pandoc --slide-level 2  -t revealjs -s -o 04.html 04.md   -V revealjs-url=./reveal.js
	pandoc --slide-level 2  -t revealjs -s -o 05.html 05.md   -V revealjs-url=./reveal.js
	pandoc -s --self-contained  -c practico.css --toc -o 06.html 06.md  
	pandoc --slide-level 2  -t revealjs -s -o 07.html 07.md   -V revealjs-url=./reveal.js
	pandoc --slide-level 2  -t revealjs -s -o 08.html 08.md   -V revealjs-url=./reveal.js
	pandoc --slide-level 2  -t revealjs -s -o 09.html 09.md   -V revealjs-url=./reveal.js
	pandoc parcial3.md -o parcial3.docx
.PHONY: scp
scp:
	scp index.html  0*.html russell:public_html/md20/

.PHONY: practico
practico:
#	pandoc 01.md -o 01.html -s --self-contained  -c practico.css --toc
#	pandoc 02.md -o 02.html -s --self-contained  -c practico.css --toc
#	pandoc 03.md -o 03.html -s --self-contained  -c practico.css --toc
#	pandoc 04.md -o 04.html -s --self-contained  -c practico.css --toc
#	pandoc 05.md -o 05.html -s --self-contained  -c practico.css --toc
#	pandoc 06.md -o 06.html -s --self-contained  -c practico.css --toc
#	pandoc 07.md -o 07.html -s --self-contained  -c practico.css --toc
#	pandoc 10.md -o 10.html -s --self-contained  -c practico.css --toc
#	pandoc recup3.md -o recup3.html -s --self-contained  -c practico.css
#	pandoc proyecto.md -o proyecto.html -s --self-contained  -c practico.css --toc
