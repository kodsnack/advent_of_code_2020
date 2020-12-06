input = File.read("input")

# input = <<~EOS
# 1721
# 979
# 366
# 299
# 675
# 1456
# EOS

numbers = input.lines.map(&:to_i)

def now; Process.clock_gettime(Process::CLOCK_MONOTONIC); end

start = now

catch :done do
  for index1 in 0...(numbers.length - 1)
    number1 = numbers[index1]
    for index2 in (index1 + 1)...numbers.length
      number2 = numbers[index2]
      if (number1 + number2) == 2020
        puts number1 * number2
        throw :done
      end
    end
  end
end

catch :done do
  for index1 in 0...(numbers.length - 2)
    number1 = numbers[index1]
    for index2 in (index1 + 1)...(numbers.length - 1)
      number2 = numbers[index2]
      for index3 in (index2 + 1)...numbers.length
        number3 = numbers[index3]
        if (number1 + number2 + number3) == 2020
          puts number1 * number2 * number3
          throw :done
        end
      end
    end
  end
end

puts now - start

puts

start = now

class Array
  def each_2(&block)
    each_n(2, &block)
  end

  def each_3(&block)
    each_n(3, &block)
  end

  def each_n(n, startIndex: 0, &block)
    for indexI in startIndex..(self.length - n)
      elementI = self[indexI]
      if n == 1
        yield elementI
      else
        each_n(n - 1, startIndex: startIndex + 1) do |*elements|
          yield *elements, elementI
        end
      end
    end
  end
end

numbers.each_2 do |number1, number2|
  if (number1 + number2) == 2020
    puts number1 * number2
    break
  end
end

numbers.each_3 do |number1, number2, number3|
  if (number1 + number2 + number3) == 2020
    puts number1 * number2 * number3
    break
  end
end

puts now - start
