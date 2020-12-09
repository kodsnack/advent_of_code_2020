
valid_codes = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] # optional cid

valid_parts = 0
valid_pws = 0
with open('input.txt', 'r') as f:
    for line in f:
        if line.strip():
            parts = line.strip().split(' ')
            for part in parts:
                code = part.split(':')
                if code[0] in valid_codes:
                    valid_parts += 1
        else:
            if valid_parts == len(valid_codes):
                valid_pws += 1
            valid_parts = 0

f.close()

if valid_parts == len(valid_codes):
    valid_pws += 1

print(f'Found {valid_pws} valid pw''s')
