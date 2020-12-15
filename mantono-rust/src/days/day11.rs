use std::{collections::HashMap, fmt::Display, slice::Iter};

/// All decisions are based on the number of occupied seats adjacent to a given seat
/// (one of the eight positions immediately up, down, left, right, or diagonal from the seat).
/// The following rules are applied to every seat simultaneously:
///
///  - If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
///  - If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
///  - Otherwise, the seat's state does not change.
///
/// Floor (.) never changes; seats don't move, and nobody sits on the floor.
pub fn first(input: String) -> String {
    let (grid, bounds) = transform(input);
    update(grid, 0, 4, bounds).to_string()
}

/// People don't just care about adjacent seats - they care about the first seat they can see in
/// each of those eight directions!
///  Now, instead of considering just the eight immediately adjacent seats,
/// consider the first seat in each of those eight directions.
pub fn second(input: String) -> String {
    let (grid, bounds) = transform(input);
    update(grid, 0, 5, bounds).to_string()
}

fn transform(input: String) -> (Vec<Pos>, Bounds) {
    let mut width: usize = 97;
    let grid: Vec<Pos> = input
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .inspect(|line| width = line.len())
        .map(|line| line.chars())
        .flatten()
        .map(Pos::from)
        .collect::<Vec<Pos>>();

    let height: usize = grid.len() / width;
    let bounds = Bounds { width, height };
    (grid, bounds)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Bounds {
    pub width: usize,
    pub height: usize,
}

impl Bounds {
    pub fn contains(&self, coord: &Coord) -> bool {
        coord.x() < self.width && coord.y() < self.height
    }

    pub fn mov(&self, coord: &Coord, dir: Direction) -> Option<Coord> {
        let x: Option<usize> = match dir {
            Direction::NorthEast => coord.x().checked_add(1),
            Direction::East => coord.x().checked_add(1),
            Direction::SouthEast => coord.x().checked_add(1),
            Direction::SouthWest => coord.x().checked_sub(1),
            Direction::West => coord.x().checked_sub(1),
            Direction::NorthWest => coord.x().checked_sub(1),
            _ => Some(coord.x),
        };

        let y: Option<usize> = match dir {
            Direction::NorthWest => coord.y().checked_sub(1),
            Direction::North => coord.y().checked_sub(1),
            Direction::NorthEast => coord.y().checked_sub(1),
            Direction::SouthWest => coord.y().checked_add(1),
            Direction::South => coord.y().checked_add(1),
            Direction::SouthEast => coord.y().checked_add(1),
            _ => Some(coord.y()),
        };

        match (x, y) {
            (Some(x), Some(y)) => Some(Coord::from(x, y)),
            _ => None,
        }
    }

    pub fn get_index(&self, coord: &Coord) -> Option<usize> {
        if !self.contains(coord) {
            None
        } else {
            Some(self.width * coord.y() + coord.x())
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
enum Direction {
    North,
    NorthWest,
    West,
    SouthWest,
    South,
    SouthEast,
    East,
    NorthEast,
}

impl Direction {
    pub fn iterator() -> Iter<'static, Direction> {
        static DIRECTIONS: [Direction; 8] = [
            Direction::North,
            Direction::NorthWest,
            Direction::West,
            Direction::SouthWest,
            Direction::South,
            Direction::SouthEast,
            Direction::East,
            Direction::NorthEast,
        ];
        DIRECTIONS.iter()
    }
}

fn update(grid: Vec<Pos>, occupied: usize, occ_threshold: usize, bounds: Bounds) -> usize {
    let grid_now = grid
        .iter()
        .enumerate()
        .inspect(|(i, pos)| {
            if i % bounds.width == bounds.width - 1 {
                print!("{}\n", pos)
            } else {
                print!("{}", pos)
            }
        })
        .map(|(i, pos)| update_pos(&grid, i, occ_threshold, pos, &bounds))
        .collect::<Vec<Pos>>();

    let occupied_now: usize = count_occupied(&grid_now);

    if occupied == occupied_now {
        occupied_now
    } else {
        println!("Occupied seats: {}\n", occupied_now);
        update(grid_now, occupied_now, occ_threshold, bounds)
    }
}

fn update_pos(grid: &Vec<Pos>, i: usize, occ_threshold: usize, pos: &Pos, bounds: &Bounds) -> Pos {
    if let Pos::Floor = pos {
        return *pos;
    }

    let adj_occ = neighbours(grid, i, bounds);
    pos.update(adj_occ, occ_threshold)
}

///    C1   C2    C3
/// |-----------------|
/// | -98 | -97 | -96 | R1
/// | -1  |  X  | +1  | R2
/// | +96 | +97 | +98 | R3
/// |-----------------|
fn neighbours(grid: &Vec<Pos>, i: usize, bounds: &Bounds) -> usize {
    let coord = Coord::from_bounds(i, bounds);

    Direction::iterator()
        .map(|dir| is_occupied(grid, bounds, coord, *dir))
        .count()
}

fn is_occupied(grid: &Vec<Pos>, bounds: &Bounds, coord: Coord, dir: Direction) -> bool {
    let i: usize = bounds.get_index(&coord);
    match grid.get(i) {
        None => false,
        Some(Pos::Occupied) => true,
        _ => match bounds.mov(&coord, dir) {
            None => false,
            Some(c) => is_occupied(grid, bounds, c, dir),
        },
    }
}

fn is_completed(map: &Vec<Option<Pos>>) -> bool {
    map.iter().all(|pos: &Option<Pos>| match pos {
        None | Some(Pos::Occupied) => true,
        Some(Pos::Floor) | Some(Pos::Empty) => false,
    })
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Coord {
    x: usize,
    y: usize,
}

impl Coord {
    pub fn from_bounds(i: usize, bounds: &Bounds) -> Coord {
        let x = i % bounds.width;
        let y = i / bounds.width;
        Coord { x, y }
    }

    pub fn from(x: usize, y: usize) -> Coord {
        Coord { x, y }
    }

    fn index(&self, bounds: &Bounds) -> usize {
        self.x + self.y * bounds.width
    }

    fn x(&self) -> usize {
        self.x
    }
    fn y(&self) -> usize {
        self.y
    }
}

fn count_occupied(grid: &Vec<Pos>) -> usize {
    grid.iter().filter(|p| p.is_occupied()).count()
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
enum Pos {
    Floor,
    Empty,
    Occupied,
}

impl Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c: char = match self {
            Pos::Floor => '.',
            Pos::Empty => 'L',
            Pos::Occupied => '#',
        };
        write!(f, "{}", c)
    }
}

impl Pos {
    pub fn from(c: char) -> Pos {
        match c {
            '.' => Pos::Floor,
            'L' => Pos::Empty,
            '#' => Pos::Occupied,
            _ => panic!("Invalid char: {}", c),
        }
    }

    pub fn update(&self, adj_occ: usize, threshold: usize) -> Self {
        match self {
            Pos::Occupied if adj_occ >= threshold => Pos::Empty,
            Pos::Empty if adj_occ == 0 => Pos::Occupied,
            _ => self.clone(),
        }
    }

    pub fn is_occupied(&self) -> bool {
        match self {
            Pos::Occupied => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::second;

    #[test]
    fn test_part2() {
        let input = r"
        L.LL.LL.LL
        LLLLLLL.LL
        L.L.L..L..
        LLLL.LL.LL
        L.LL.LL.LL
        L.LLLLL.LL
        ..L.L.....
        LLLLLLLLLL
        L.LLLLLL.L
        L.LLLLL.LL
        ";

        assert_eq!("26", &second(input.to_string()));
    }
}
