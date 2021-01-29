BASEDIR=$1

PUBLIC="Slides080.md"

for f in $PUBLIC
do
 cp $f $1/
 pandoc -w html -s --self-contained -c style.css $f -o $1/`basename $f .md`.html
 pandoc -w slidy -s --self-contained -c style.css $f -o $1/`basename $f .md`_slidy.html
 pandoc -w dzslides -s --self-contained  $f -o $1/`basename $f .md`_dzslides.html
# pandoc -w dzslides -s --mathjax  $f -o $1/`basename $f .md`_mathjax.html
done

#PUBLIC2="ladner.md cooklevin.md"
PUBLIC2="Slides025.md Slides060.md"
#
for f in $PUBLIC2
do
 pandoc -w html -s $f -o $1/`basename $f .md`.html
done
