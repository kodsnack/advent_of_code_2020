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
    let vec: Vec<Pos> = input
        .lines()
        .map(|line| line.chars())
        .flatten()
        .map(Pos::from)
        .collect::<Vec<Pos>>();

    let area: Grid<Pos> = Grid::from(vec, 97).unwrap();

    loop {
        let updated_area: &Vec<Pos> = &area
            .map(|(x, y)| {
                let occ_seats: usize = occupied_seats(&area, x, y);
                area.get(x, y).unwrap().update(occ_seats)
            })
            .collect::<Vec<Pos>>();
    }

    "".to_string()
}

fn update_area(mut area: Grid<Pos>) -> usize {
    let mut changes: usize = 0;
    let area_next = area
        .map(|(x, y)| {
            let occ_seats: usize = occupied_seats(&area, x, y);
            let seat_before: &Pos = area.get(x, y).unwrap();
            let seat_after: Pos = seat_before.update(occ_seats);
            if *seat_before != seat_after {
                changes += 1
            };
            seat_after
        })
        .collect::<Vec<Pos>>();

    if changes == 0 {
        area.reset_iter();
        area.filter(|(x, y)| area.get(*x, *y).unwrap().is_occupied())
            .count()
    } else {
        let grid = Grid::from(area_next, 97).unwrap();
        update_area(grid)
    }
}

fn occupied_seats(area: &Grid<Pos>, x: usize, y: usize) -> usize {
    area.neighbours(x, y)
        .iter()
        .filter(|p| p.is_occupied())
        .count()
}

struct Grid<T: Sized> {
    elements: Vec<T>,
    width: usize,
    counter: usize,
}

impl<T: Sized> Iterator for Grid<T> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if self.counter >= self.elements.len() {
            None
        } else {
            let x = self.counter % self.width;
            let y = self.counter / self.width;
            self.counter += 1;
            Some((x, y))
        }
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
enum Pos {
    Floor,
    Empty,
    Occupied,
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

impl<T: Sized> Grid<T> {
    pub fn from(elements: Vec<T>, width: usize) -> Result<Grid<T>, String> {
        if elements.len() % width != 0 {
            Err(format!(
                "Cannot create Grid with width {} from vector of length {}",
                width,
                elements.len()
            ))
        } else {
            Ok(Grid {
                elements,
                width,
                counter: 0,
            })
        }
    }

    pub fn new(width: usize, height: usize) -> Grid<T> {
        let vec: Vec<T> = Vec::with_capacity(width * height);
        Grid {
            elements: vec,
            width,
            counter: 0,
        }
    }

    pub fn get<'a>(&'a self, x: usize, y: usize) -> Option<&'a T> {
        let i: usize = x + y * self.width;
        self.elements.get(i)
    }

    pub fn reset_iter(&mut self) {
        self.counter = 0;
    }

    pub fn neighbours<'a>(&'a self, x: usize, y: usize) -> Vec<&'a T> {
        let n0 = (x - 1, y - 1);
        let n1 = (x, y - 1);
        let n2 = (x + 1, y - 1);

        let n3 = (x - 1, y);
        let n4 = (x + 1, y);

        let n5 = (x - 1, y + 1);
        let n6 = (x, y + 1);
        let n7 = (x + 1, y + 1);

        let pos: Vec<(usize, usize)> = vec![n0, n1, n2, n3, n4, n5, n6, n7];
        let x_range = 0..self.width;
        let y_range = 0..self.height();

        pos.iter()
            .filter(|(x, y)| x_range.contains(x) && y_range.contains(y))
            .filter_map(|(x, y)| self.get(*x, *y))
            .collect::<Vec<&'a T>>()
    }

    fn height(&self) -> usize {
        self.elements.len() / self.width
    }
}

pub fn second(input: String) -> String {
    input
}
