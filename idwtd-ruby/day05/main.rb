input = File.read("input")

# input = <<~EOS
# FBFBBFFRLR
# BFFFBBFRRR
# FFFBBBFRRR
# BBFFBBFRLL
# EOS

def seat_id(seat)
  rows = 128
  row_min = 0
  row_max = rows - 1

  columns = 8
  column_min = 0
  column_max = columns - 1

  seat.each_char do |char|
    case char
    when 'F'
      rows /= 2
      row_max -= rows
    when 'B'
      rows /= 2
      row_min += rows
    when 'L'
      columns /= 2
      column_max -= columns
    when 'R'
      columns /= 2
      column_min += columns
    end
  end

  row_min * 8 + column_min
end

id_list = input.strip.lines.map(&method(:seat_id))

puts id_list.max

for id in id_list.min..id_list.max
  unless id_list.include? id
    puts id
    break
  end
end
