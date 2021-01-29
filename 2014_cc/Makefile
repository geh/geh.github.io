.SUFFIXES: .md .html

all: *.html


index.html: index.md
	pandoc -w html   -s  -c style.css index.md -o index.html

Ejercicios1.html: Ejercicios1.md
	pandoc -w html   -s  -c style.css Ejercicios1.md -o Ejercicios1.html

Ejercicios2.html: Ejercicios2.md
	pandoc -w html   -s  -c style.css Ejercicios2.md -o Ejercicios2.html

Ejercicios3.html: Ejercicios3.md
	pandoc -w html   -s  -c style.css Ejercicios3.md -o Ejercicios3.html

Ejercicios4.html: Ejercicios4.md
	pandoc -w html   -s  -c style.css Ejercicios4.md -o Ejercicios4.html

.md.html:
	pandoc -w slidy  -s  -c style.css $< -o $@ 
	pandoc -w html   -s  -c style.css $< -o handout_$@ 

scp:
	scp index.* russell:public_html/cc14/
	scp *Slides040* russell:public_html/cc14/md/
	scp *Slides080* russell:public_html/cc14/md/
	scp Ejercicios4* russell:public_html/cc14/md/


