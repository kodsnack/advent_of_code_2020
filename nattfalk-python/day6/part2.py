group_answers = []

def get_unique_answers():
    everyones_answer = []
    for idx, ga in enumerate(group_answers):
        if idx > 0:
            everyones_answer = list(set(everyones_answer) & set(ga))
        else:
            everyones_answer = list(ga)
    return len(everyones_answer)

answered_questions = 0
with open('input.txt', 'r') as f:
    for line in f:
        line = line.strip()
        if line:
            group_answers.append(list(line))
        else:
            answered_questions += get_unique_answers()
            group_answers = []

if len(group_answers) > 0:
    answered_questions += get_unique_answers()

print(f'Total {answered_questions}')
