
file_lines = open("input.txt", "r").readlines()

valid_passwords = 0
for l in file_lines:
    parts = l.rstrip().split(":")

    char_rules = parts[0].split(" ")[0]
    min_count = int(char_rules.split("-")[0])
    max_count = int(char_rules.split("-")[1])

    pw_char = parts[0].split(' ')[1].strip()

    char_count = parts[1].strip().count(pw_char)
    if char_count >= min_count and char_count <= max_count:
        valid_passwords += 1

print(f'Found {valid_passwords} valid pw''s')
