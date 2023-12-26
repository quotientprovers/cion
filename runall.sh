#!/bin/sh

BENCHES="evenodd counter msq treiber descriptor listset"

for f in $BENCHES; do
    # rm cache; touch digests
    ./main.native bench/$f.c 2>&1 > results.$f.txt
    dot -Tpdf bench/$f.c.dot -o bench/$f.c.pdf
    echo "--- $f complete ---"
    echo "Resulting layers: bench/$f.c.tex"
    echo "Resulting logfile: results.$f.txt"
    echo "Resulting tex table line:"
    grep RESULT results.$f.txt
done

cp bench/*.c.pdf ~/Desktop/Parallels\ Shared\ Folders/Home/Desktop/