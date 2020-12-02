#!/bin/bash

testmode=false
file=$1
if [[ $1 == "test" ]]; then
	file="b.test.input"
	testmode="true"
fi

mapfile -t input < "$file"

for first in "${input[@]}"; do
	for second in "${input[@]}"; do
		for third in "${input[@]}"; do
			if (( first + second + third == 2020 )); then
				result=$(( first * second * third ))
				echo -n "Found '$result'"
				if [[ $testmode == "true" ]]; then
					if [[ $result == $(< b.test.solution) ]]; then
						echo " - matches given solution."
					else
						echo " - does NOT match given solution."
					fi
				fi
				exit
			fi
		done
	done
done
