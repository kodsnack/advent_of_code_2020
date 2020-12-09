
file_lines = open("input.txt", "r").readlines()

valid_passwords = 0
for l in file_lines:
    parts = l.rstrip().split(":")

    char_rules = parts[0].split(" ")[0]
    p1 = int(char_rules.split("-")[0]) - 1
    p2 = int(char_rules.split("-")[1]) - 1
    pw_char = parts[0].split(' ')[1].strip()
    pw = parts[1].strip()

    count = 1 if pw[p1] == pw_char else 0
    count += 1 if pw[p2] == pw_char else 0
    if count == 1:
        valid_passwords += 1

print(f'Found {valid_passwords} valid pw''s')
