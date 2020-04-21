#!/bin/bash

F=files.txt

if [ -f "stats_sal-tot.csv" ]; then
	rm -f stats_sal-tot.csv
fi

cat "stats_sal-201819.csv" | grep "Signed Using" >> stats_sal-tot.csv
while read F; do
	#echo $F
	cat $F | wc -l
    cat $F | grep -v "Signed Using" >> stats_sal-tot.csv
    cat stats_sal-tot.csv | wc -l
done <files.txt