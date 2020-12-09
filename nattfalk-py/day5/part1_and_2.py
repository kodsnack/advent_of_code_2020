def get_row(code):
    total_rows = 128
    row = 0
    for c in code:
        total_rows = int(total_rows / 2)
        row += 0 if c == "F" else total_rows
    #print(row)
    return row

def get_col(code):
    total_cols = 8
    col = 0
    for c in code:
        total_cols = int(total_cols / 2)
        col += 0 if c == "L" else total_cols
    #print(col)
    return col

highest_seat_id = 0
used_seats_list = [0 for i in range(128*8)]
with open('input.txt', 'r') as f:
    for line in f:
        #print(line[:7])
        row = get_row(line[:7])
        #print(line[7:])
        col = get_col(line[7:])
        highest_seat_id = max(highest_seat_id, row * 8 + col)
        used_seats_list[row * 8 + col] = 1

f.close()

print(f'Highest seat-ID is {highest_seat_id}')

free_seat_id = 0
for i in range(1, 128*8-1):
    if used_seats_list[i-1] == 1 and used_seats_list[i] == 0 and used_seats_list[i+1] == 1:
        free_seat_id = i
        break
print(f'Free seat-ID is {free_seat_id}')
