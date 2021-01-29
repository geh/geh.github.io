.PHONY: index
index:
	pandoc -w html -s -c practico.css parcial01.md -o parcial01.html --self-contained --toc --toc-depth 1 -N 

.PHONY: scp
scp:
	scp parcial01.html russell:public_html/pd20/p01_.html

