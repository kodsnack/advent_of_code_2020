input = File.read("input")

# input = <<~EOS
# 1-3 a: abcde
# 1-3 b: cdefg
# 2-9 c: ccccccccc
# EOS

sum1, sum2 = 0, 0
input.strip.lines.each do |line|
  m = line.strip.match(/([0-9]+)-([0-9]+) (.): (.+)/)
  raise if m.nil?
  i, j, char, password = m.captures
  i, j = [i, j].map(&:to_i)

  count = password.count(char)
  if count >= i && count <= j
    sum1 += 1
  end

  c1, c2 = password[i - 1], password[j - 1]
  b1, b2 = c1 == char, c2 == char
  # p xor q == (p and not q) or (not p and q)
  if (b1 && !b2) || (!b1 && b2)
    sum2 += 1
  end
end
puts sum1, sum2
