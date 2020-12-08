struct Instruction {
    operation: String,
    argument: i32,
    visited: bool,
}

pub fn calculate(code: &str) -> i32 {
    attempt_run(code)
}

fn attempt_run(s: &str) -> i32 {
    let mut instructions: Vec<Instruction> = parse_instructions(s);
    let instructions_len = instructions.len();
    for index in 0..instructions_len {
        instructions = parse_instructions_patch(s, index);
        let mut sum: i32 = 0;
        let mut i: i32 = 0;
        loop {
            let usize_i = i as usize;
            if usize_i == instructions_len {
                return sum;
            }
            if usize_i < 0 {
                break;
            }
            if instructions[usize_i].visited {
                break;
            }
            instructions[usize_i].visited = true;
            let operation = instructions[usize_i].operation.as_str();
            i += match operation {
                "acc" => {
                    sum += instructions[usize_i].argument;
                    1
                },
                "jmp" => instructions[usize_i].argument,
                "nop" => 1,
                _ => panic!("Unknown instruction yo"),
            }
        }
    }
    panic!("No valid program found");
}

fn parse_instructions(s: &str) -> Vec<Instruction> {
    s.lines().map(|l| parse_instruction(l)).collect()
}
fn parse_instructions_patch(s: &str, patch: usize) -> Vec<Instruction> {
    let mut index = 0;
    s.lines().map(|l| {
        let mut parsed = parse_instruction(l);
        if index == patch {
            let operation = parsed.operation.as_str();
            parsed.operation = match operation {
                "jmp" => "nop".to_string(),
                "nop" => "jmp".to_string(),
                _ => operation.to_string(),
            };
        }
        index += 1;
        parsed
    }).collect()
}

fn parse_instruction(s: &str) -> Instruction {
    let operation = s[..3].to_string();
    let operand = &s[4..5];
    let mul = match operand {
        "-" => -1,
        _ => 1,
    };
    let argument: i32 = s[5..].parse::<i32>().unwrap() * mul;
    Instruction {
        operation,
        argument,
        visited: false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static TESTDATA: &str = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6";

#[test]
fn test_parse_instruction() {
    let res = parse_instruction("acc +1");
    let cmp = Instruction{
        operation: "acc".to_string(),
        argument: 1,
        visited: false,
    };

    assert_eq!(res.operation, cmp.operation);
    assert_eq!(res.argument, cmp.argument);
    assert_eq!(res.visited, cmp.visited);
}
#[test]
fn test_parse_instruction_neg() {
    let res = parse_instruction("jmp -4");
    let cmp = Instruction{
        operation: "jmp".to_string(),
        argument: -4,
        visited: false,
    };
    assert_eq!(res.operation, cmp.operation);
    assert_eq!(res.argument, cmp.argument);
    assert_eq!(res.visited, cmp.visited);
}
#[test]
fn test_parse_instructions() {
    let res = parse_instructions(TESTDATA);
    assert_eq!(res[0].operation, "nop".to_string());
}
#[test]
fn test_attempt_run() {
    assert_eq!(attempt_run(TESTDATA), 8);
}
}