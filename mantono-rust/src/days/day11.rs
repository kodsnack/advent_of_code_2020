use std::fmt::Display;

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
    let grid: Vec<Pos> = input
        .lines()
        .map(|line| line.chars())
        .flatten()
        .map(Pos::from)
        .collect::<Vec<Pos>>();

    update(grid, 0).to_string()
}

const WIDTH: usize = 97;
const HEIGHT: usize = 91;

fn update(grid: Vec<Pos>, occupied: usize) -> usize {
    let grid_now = grid
        .iter()
        .enumerate()
        .inspect(|(i, pos)| {
            if i % WIDTH == WIDTH - 1 {
                print!("{}\n", pos)
            } else {
                print!("{}", pos)
            }
        })
        .map(|(i, pos)| update_pos(&grid, i, pos))
        .collect::<Vec<Pos>>();

    let occupied_now: usize = count_occupied(&grid_now);

    if occupied == occupied_now {
        occupied_now
    } else {
        println!("Occupied seats: {}\n", occupied_now);
        update(grid_now, occupied_now)
    }
}

fn update_pos(grid: &Vec<Pos>, i: usize, pos: &Pos) -> Pos {
    if let Pos::Floor = pos {
        return *pos;
    }

    let neighbours: Vec<usize> = neighbours(i);
    let adj_occ = neighbours
        .iter()
        .map(|index: &usize| grid.get(*index).unwrap())
        .filter(|p: &&Pos| p.is_occupied())
        .count();

    pos.update(adj_occ)
}

///    C1   C2    C3
/// |-----------------|
/// | -98 | -97 | -96 | R1
/// | -1  |  X  | +1  | R2
/// | +96 | +97 | +98 | R3
/// |-----------------|
fn neighbours(i: usize) -> Vec<usize> {
    let coord = Coord::from(i);

    vec![
        coord.up().left(),
        coord.up(),
        coord.up().right(),
        coord.left(),
        coord.right(),
        coord.down().left(),
        coord.down(),
        coord.down().right(),
    ]
    .iter()
    .filter_map(|c| *c)
    .map(|c| c.index())
    .collect::<Vec<usize>>()
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Coord {
    x: usize,
    y: usize,
}

trait Navigable {
    type Item;

    fn left(&self) -> Option<Self::Item>;
    fn right(&self) -> Option<Self::Item>;
    fn up(&self) -> Option<Self::Item>;
    fn down(&self) -> Option<Self::Item>;
}

impl Navigable for Coord {
    type Item = Coord;

    fn up(&self) -> Option<Coord> {
        if self.y == 0 {
            None
        } else {
            Some(Coord {
                x: self.x,
                y: self.y - 1,
            })
        }
    }

    fn down(&self) -> Option<Coord> {
        if self.y == HEIGHT - 1 {
            None
        } else {
            Some(Coord {
                x: self.x,
                y: self.y + 1,
            })
        }
    }

    fn left(&self) -> Option<Coord> {
        if self.x % WIDTH == 0 {
            None
        } else {
            Some(Coord {
                x: self.x - 1,
                y: self.y,
            })
        }
    }

    fn right(&self) -> Option<Coord> {
        if self.x % WIDTH == WIDTH - 1 {
            None
        } else {
            Some(Coord {
                x: self.x + 1,
                y: self.y,
            })
        }
    }
}

impl Coord {
    pub fn from(i: usize) -> Coord {
        let x = i % WIDTH;
        let y = i / WIDTH;
        Coord { x, y }
    }

    fn index(&self) -> usize {
        self.x + self.y * WIDTH
    }
}

impl Navigable for Option<Coord> {
    type Item = Coord;

    fn left(&self) -> Option<Self::Item> {
        match self {
            None => None,
            Some(c) => c.left(),
        }
    }

    fn right(&self) -> Option<Self::Item> {
        match self {
            None => None,
            Some(c) => c.right(),
        }
    }

    fn up(&self) -> Option<Self::Item> {
        match self {
            None => None,
            Some(c) => c.up(),
        }
    }

    fn down(&self) -> Option<Self::Item> {
        match self {
            None => None,
            Some(c) => c.down(),
        }
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

    pub fn update(&self, adj_occ: usize) -> Self {
        match self {
            Pos::Occupied if adj_occ >= 4 => Pos::Empty,
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

pub fn second(input: String) -> String {
    input
}
