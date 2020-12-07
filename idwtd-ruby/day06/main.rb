require 'set'

input = File.read("input")

# input = <<~EOS
# abc

# a
# b
# c

# ab
# ac

# a
# a
# a
# a

# b
# EOS

total = 0
total2 = 0
input.strip.split("\n\n").each do |group|
  current = Set.new
  all = []
  group.lines.each do |line|
    answered = line.strip.split('')
    current += answered
    all << answered
  end
  total += current.length
  total2 += all.reduce(&:&).length
end
puts total
puts total2
