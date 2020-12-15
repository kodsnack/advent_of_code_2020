// use super::rotate;

#[derive(Debug)]
pub struct Location {
    pub x: i32,
    pub y: i32,
    pub direction: char, // 'N' | 'E' | 'S' | 'W',
}

#[derive(Debug)]
pub struct Instruction {
    pub steps: i32,
    pub direction: char, // 'N' | 'S' | 'E' | 'W' | 'L' | 'R'
}
impl PartialEq for Instruction {
    fn eq(&self, other: &Self) -> bool {
        self.steps == other.steps && self.direction == other.direction
    }
}
impl Eq for Instruction {}

pub fn parse_file(s: &str) -> Vec<Instruction> {
    s.lines().map(|line| {
        Instruction {
            direction: line.chars().nth(0).unwrap(),
            steps: line[1..].parse().unwrap(),
        }
    }).collect()
}

pub fn follow_instruction(location: &Location, instruction: &Instruction) -> Location {
    match instruction.direction {
        'L' => Location {
            direction: rotate_ccw(location.direction, instruction.steps),
            x: location.x,
            y: location.y,
            },
        'R' => Location {
            direction: rotate_cw(location.direction, instruction.steps),
            x: location.x,
            y: location.y,
        },
        'N' | 'S' | 'E' | 'W' => strafe_direction(&location, &instruction),
        'F' => strafe_direction(&location, &Instruction {
            direction: location.direction,
            steps: instruction.steps,
        }),
        _ => panic!("Unknown instruction direction {}", instruction.direction),
    }
}

fn strafe_direction(location: &Location, instruction: &Instruction) -> Location {
    let mut x = location.x;
    let mut y = location.y;
    match instruction.direction {
        'N' => y -= instruction.steps,
        'S' => y += instruction.steps,
        'E' => x += instruction.steps,
        'W' => x -= instruction.steps,
        _ => panic!("Unknown direction {}", instruction.direction),
    }
    Location {
        direction: location.direction,
        x,
        y,
    }
}

fn rotate_ccw(current_direction: char, steps: i32) -> char {
    match steps {
        90 => rotate90(rotate90(rotate90(current_direction))),
        180 => rotate90(rotate90(current_direction)),
        270 => rotate90(current_direction),
        360 | 0 => current_direction,
        _ => panic!("Can't rotate {} degrees", steps),
    }
}
fn rotate_cw(current_direction: char, steps: i32) -> char {
    match steps {
        90 => rotate90(current_direction),
        180 => rotate90(rotate90(current_direction)),
        270 => rotate90(rotate90(rotate90(current_direction))),
        360 | 0 => current_direction,
        _ => panic!("Can't rotate {} degrees", steps),
    }
}
fn rotate90(current_direction: char) -> char {
    match current_direction {
        'N' => 'E',
        'E' => 'S',
        'S' => 'W',
        'W' => 'N',
        _ => panic!("{} is not a direction", current_direction),
    }
}
pub fn manhattan_distance(location1: &Location, location2: &Location) -> i32 {
    let x: i32;
    let y: i32;
    if location1.x > location2.x {
        x = location1.x - location2.x;
    } else {
        x = location2.x - location1.x;
    }
    if location1.y > location2.y {
        y = location1.y - location2.y;
    } else {
        y = location2.y - location1.y;
    }
    x + y
}

#[cfg(test)]
mod tests {
    use super::*;

    static TESTDATA: &str = "F10
N3
F7
R90
F11";
    #[test]
    fn test_rotate90() {
        assert_eq!(rotate90('N'), 'E');
        assert_eq!(rotate_cw('N', 90), 'E');
        assert_eq!(rotate_cw('N', 180), 'S');
        assert_eq!(rotate_cw('N', 270), 'W');
        assert_eq!(rotate_ccw('N', 90), 'W');
        assert_eq!(rotate_ccw('N', 180), 'S');
        assert_eq!(rotate_ccw('N', 270), 'E');
    }
    #[test]
    fn test_strafe_direction() {
        let mut ship = Location {
            x: 0,
            y: 0,
            direction: 'E', // default
        };
        ship = strafe_direction(&ship, &Instruction {
            direction: 'N',
            steps: 100,
        });
        assert_eq!(ship.direction, 'E');
        assert_eq!(ship.x, 0);
        assert_eq!(ship.y, -100);
    }
    #[test]
    fn test_parse_file() {
        let instructions = parse_file(TESTDATA);
        assert_eq!(instructions[0], Instruction{direction: 'F', steps: 10});
    }
    #[test]
    fn test_instructions() {
        let instructions = parse_file(TESTDATA);
        let mut ship = Location {
            x: 0,
            y: 0,
            direction: 'E', // default
        };
        instructions.iter().for_each(|instruction|ship = follow_instruction(&ship, instruction));
        assert_eq!(ship.x, 17);
        assert_eq!(ship.y, 8);
    }
    #[test]
    fn test_manhattan() {
        let instructions = parse_file(TESTDATA);
        let start = Location {
            x: 0,
            y: 0,
            direction: 'E', // default
        };
        let mut ship = Location {
            x: 0,
            y: 0,
            direction: 'E', // default
        };
        instructions.iter().for_each(|instruction|ship = follow_instruction(&ship, instruction));
        assert_eq!(manhattan_distance(&ship, &start), 25);
        assert_eq!(manhattan_distance(&start, &ship), 25);
    }
}
