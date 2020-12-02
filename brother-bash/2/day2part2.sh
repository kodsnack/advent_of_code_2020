#!/bin/bash

if [[ -z $1 ]]; then
	echo "Either specify test or a input file."
	exit 1
fi

file=$1
testmode="false"
if [[ $1 == "test" ]]; then
	file=b.test.input
	testmode="true"
fi

count=0

mapfile -t passwords < "$file"

for line in "${passwords[@]}"; do
	policy=${line%:*}
    positions=${policy% *}
	first=$((${positions%-*}-1))
	second=$((${positions#*-}-1))
	letter=${policy#* }
	password=${line#*: }

	if [[ $password != *$letter* ]]; then
		continue
	fi

	if [[ ${password:$first:1} == $letter ]] && [[ ${password:$second:1} == $letter ]]; then
		continue
	fi

	if [[ ${password:$first:1} == $letter ]] && [[ ${password:$second:1} != $letter ]]; then
		count=$((count+1))
	elif [[ ${password:$first:1} != $letter ]] && [[ ${password:$second:1} == $letter ]]; then
		count=$((count+1))
	else
		continue
	fi
done

echo "$count passwords are valid."
if [[ $testmode == "true" ]]; then
	solution=$(< b.test.solution)
	if [[ $count != $solution ]]; then
		echo "...but that's not correct. It's supposed to be $solution."
	fi
fi
