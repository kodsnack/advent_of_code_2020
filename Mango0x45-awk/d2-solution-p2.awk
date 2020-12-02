#!/bin/awk -f

{
    split($1, pos, "-")
    split($3, str, "")
    char = substr($2, 1, 1)

    # Count the frequency of char
    freq = 0
    if (str[pos[1]] == char) { freq++ }
    if (str[pos[2]] == char) { freq++ }

    if (freq == 1) { count++ }
}

END {
    print count
}
