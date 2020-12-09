preamble_size = 25

with open('input.txt','r') as f:
    values = [int(line.strip()) for line in f]

no_match = False
match_value = 0
for i in range(preamble_size, len(values)):
    preamble = values[i-preamble_size:i]
    match_value = values[i]
    for j in preamble:
        if match_value-j in preamble:
            break
    else:
        no_match = True
        break
if no_match:
    print('Did not find a match for value', match_value)

match = False
min_value = 0
max_value = 0
for i in range(0,len(values)):
    for j in range(i+2,len(values)):
        contious_set_sum = sum(values[i:j])
        if contious_set_sum == match_value:
            min_value = min(values[i:j])
            max_value = max(values[i:j])
            match = True
            break
        elif contious_set_sum > match_value:
            break
    if match:
        break

encryption_weakness = min_value + max_value
print('Encryption weakness', encryption_weakness)