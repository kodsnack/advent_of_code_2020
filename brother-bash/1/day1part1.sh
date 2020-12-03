#!/bin/bash

testmode=false
file=$1
if [[ $1 == "test" ]]; then
	file="a.test.input"
	testmode="true"
fi

mapfile -t input < "$file"

for first in "${input[@]}"; do
	for second in "${input[@]}"; do
		if (( first + second == 2020 )); then
			result=$(( first * second))
			echo -n "Found '$result'"
			if [[ $testmode == "true" ]]; then
				if [[ $result == $(< a.test.solution) ]]; then
					echo " - matches given solution."
				else
					echo " - does NOT match given solution."
				fi
			fi

			exit
		fi
	done
done
