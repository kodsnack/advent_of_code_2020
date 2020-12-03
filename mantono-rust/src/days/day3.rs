pub fn first(input: String) -> String {
    let map: Vec<Row> = transform(input);
    found_trees(&map, 3, 1).to_string()
}

fn transform(input: String) -> Vec<Row> {
    input
        .lines()
        .map(|line| line.trim())
        .filter(|line| line.len() > 0)
        .map(|row| Row {
            line: row.trim().to_string(),
        })
        .collect()
}

fn found_trees(map: &Vec<Row>, x_inc: usize, y_inc: usize) -> usize {
    let mut pos = Pos::new();
    let mut found_trees = 0;

    loop {
        pos.inc_x(x_inc);
        pos.inc_y(y_inc);

        let row: &Row = match map.get(pos.y) {
            None => break,
            Some(row) => row,
        };
        if row.has_tree(pos.x) {
            found_trees += 1
        }
    }

    found_trees
}

struct Pos {
    pub x: usize,
    pub y: usize,
}

impl Pos {
    fn new() -> Self {
        Pos { x: 0, y: 0 }
    }

    fn inc_x(&mut self, steps: usize) -> &mut Self {
        self.x += steps;
        self
    }

    fn inc_y(&mut self, steps: usize) -> &mut Self {
        self.y += steps;
        self
    }
}

struct Row {
    line: String,
}

impl Row {
    fn has_tree(&self, i: usize) -> bool {
        let i = i % self.line.len();
        self.line.chars().nth(i).unwrap() == '#'
    }
}

pub fn second(input: String) -> String {
    let offsets: Vec<(usize, usize)> = vec![(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)];
    let map: Vec<Row> = transform(input);
    offsets
        .iter()
        .map(|(x, y)| found_trees(&map, *x, *y))
        .inspect(|found| println!("{}", found))
        .product::<usize>()
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::{first, second};

    #[test]
    fn test_input_first_part() {
        let input = r#"
        ..##.........##.........##.........##.........##.........##.......
        #...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
        .#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
        ..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
        .#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
        ..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....
        .#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
        .#........#.#........#.#........#.#........#.#........#.#........#
        #.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
        #...##....##...##....##...##....##...##....##...##....##...##....#
        .#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#
        "#;

        let trees = first(input.to_string());
        assert_eq!("7", trees)
    }

    #[test]
    fn test_input_second_part() {
        let input = r#"
        ..##.........##.........##.........##.........##.........##.......
        #...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
        .#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
        ..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
        .#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
        ..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....
        .#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
        .#........#.#........#.#........#.#........#.#........#.#........#
        #.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
        #...##....##...##....##...##....##...##....##...##....##...##....#
        .#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#
        "#;

        let trees = second(input.to_string());
        assert_eq!("336", trees)
    }
}
