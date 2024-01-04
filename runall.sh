#!/bin/sh

BENCHES="evenodd counter msq treiber descriptor listset"

cd cion

for f in $BENCHES; do
    # rm cache; touch digests
    dune exec cion $f 2>&1 > results.$f.txt
    dot -Tpdf ../bench/$f.c.dot -o ../bench/$f.c.pdf
    echo "--- $f complete ---"
    echo "Logfile: results.$f.txt"
    echo "Output latex: ../bench/$f.c.tex"
    echo "Output DOT: ../bench/bench/$f.c.dot"
    echo "Output PDF: ../bench/bench/$f.c.pdf"
    echo "Output latex table line:"
    grep RESULT results.$f.txt
done

echo "=== Automata summaries ==="
ls ../bench/*.c.pdf

echo "=== Latex Table from paper: ==="
grep RESULT results.*.txt