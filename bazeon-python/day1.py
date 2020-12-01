
def get_input():
    with open("input_day1.txt", 'r') as input_file:
        numbers = input_file.readlines()
    return numbers


def find_two_numbers():
    numbers = get_input()
    for numb in numbers:
        needed = 2020 - int(numb)
        if str(needed) + '\n' in numbers:
            return int(numb) * needed


def find_three_numbers():
    numbers = [int(i) for i in get_input()]
    for num1 in numbers:
        for num2 in numbers:
            if num1 + num2 < 2020 and num1 != num2:
                for num3 in numbers:
                    if num3 != num1 and num3 != num2:
                        if num1 + num2 + num3 == 2020:
                            return num1 * num2 * num3


print(find_two_numbers())
print(find_three_numbers())
