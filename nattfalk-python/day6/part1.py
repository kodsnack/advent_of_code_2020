answered_questions = 0
with open('input.txt', 'r') as f:
    group_chars = ""
    for line in f:
        line = line.strip()
        if line:
            group_chars += line
        else:
            answered_questions += len(set(group_chars))
            group_chars = ""

if group_chars:
    answered_questions += len(set(group_chars))

print(f'Total {answered_questions}')