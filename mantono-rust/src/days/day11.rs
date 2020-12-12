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

    println!("\n");

    if occupied == occupied_now {
        occupied_now
    } else {
        //println!("Occupied seats: {}", occupied_now);
        update(grid_now, occupied_now)
    }
}

fn update_pos(grid: &Vec<Pos>, i: usize, pos: &Pos) -> Pos {
    if let Pos::Floor = pos {
        return *pos;
    }

    std::thread::sleep(std::time::Duration::from_millis(8));

    let neighbours: Vec<usize> = neighbours(i);
    let adj_occ = neighbours
        .iter()
        .map(|index: &usize| grid.get(*index).unwrap())
        .filter(|p: &&Pos| p.is_occupied())
        .count();

    let new = pos.update(adj_occ);
    //println!("{:?} [{}] => {} => {:?}", pos, i, adj_occ, new);
    new
}
///    C1   C2    C3
/// |-----------------|
/// | -98 | -97 | -96 | R1
/// | -1  |  X  | +1  | R2
/// | +96 | +97 | +98 | R3
/// |-----------------|
fn neighbours(i: usize) -> Vec<usize> {
    let (x, y) = coord(i);
    let x_range = (x.max(1) - 1)..=(x + 1);
    let y_range = (y.max(1) - 1)..=(y + 1);

    //print!("\n{} => ", i);

    vec![
        i.checked_sub(WIDTH + 1),
        i.checked_sub(WIDTH),
        i.checked_sub(WIDTH - 1),
        i.checked_sub(1),
        Some(i + 1),
        Some(i + WIDTH - 1),
        Some(i + WIDTH),
        Some(i + WIDTH + 1),
    ]
    .iter()
    .filter_map(|x| *x)
    .filter(|idx| {
        let (x_i, y_i) = coord(*idx);
        x_range.contains(&x_i) && y_range.contains(&y_i)
    })
    //.inspect(|n| print!("{} ", n))
    .collect::<Vec<usize>>()
}

fn coord(i: usize) -> (usize, usize) {
    let x = i % WIDTH;
    let y = i / WIDTH;
    (x, y)
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
