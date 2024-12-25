mkdir -p archive
for i in *.ml *.mli *.mll *.mly *.json; do cp $i archive/$i.txt;done
