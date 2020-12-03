#!/bin/bash

if [[ -z $1 ]]; then
	echo "Either specify test or a input file."
	exit 1
fi

file=$1
testmode="false"
if [[ $1 == "test" ]]; then
	file=a.test.input
	testmode="true"
fi

count=0

mapfile -t passwords < "$file"

for line in "${passwords[@]}"; do
	policy=${line%:*}
    range=${policy% *}
	min=${range%-*}
	max=${range#*-}
	letter=${policy#* }
	password=${line#*: }

	if (( min != 0 )) && [[ $password != *$letter* ]]; then
		continue
	fi

	j=0
	for ((i=0;i<${#password};i++)); do
		if [[ ${password:$i:1} == $letter ]]; then
			j=$((j+1))
		fi
	done

	if (( j >= min && j <= max )); then
		count=$((count+1))
	fi
done

echo "$count passwords are valid."
if [[ $testmode == "true" ]]; then
	solution=$(< a.test.solution)
	if [[ $count != $solution ]]; then
		echo "...but that's not correct. It's supposed to be $solution."
	fi
fi
