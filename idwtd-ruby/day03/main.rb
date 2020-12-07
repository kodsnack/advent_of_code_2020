INPUT = File.read("input")

# INPUT = <<~EOS
# ..##.......
# #...#...#..
# .#....#..#.
# ..#.#...#.#
# .#...##..#.
# ..#.##.....
# .#.#.#....#
# .#........#
# #.##...#...
# #...##....#
# .#..#...#.#
# EOS

LINES = INPUT.strip.lines.map(&:strip)

WIDTH = LINES.first.length
HEIGHT = LINES.length

def tree_count(right, down)
  x = 0
  y = 0

  tree = "#"
  tree_count = 0

  while y < HEIGHT
    if LINES[y][x] == tree
      tree_count += 1
    end
    x = (x + right) % WIDTH
    y += down
  end

  tree_count
end

puts tree_count(3, 1)

puts [
  tree_count(1,1),
  tree_count(3,1),
  tree_count(5,1),
  tree_count(7,1),
  tree_count(1,2),
].reduce(:*)
