struct Instruction {
    operation: String,
    argument: i32,
    visited: bool,
}

pub fn calculate(code: &str) -> i32 {
    let mut sum: i32 = 0;
    let mut i: i32 = 0;
    let mut instructions: Vec<Instruction> = parse_instructions(code);

    loop {
        let usize_i = i as usize;
        if instructions[usize_i].visited {
            return sum;
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

fn parse_instructions(s: &str) -> Vec<Instruction> {
    s.lines().map(|l| parse_instruction(l)).collect()
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
    fn test_calculate() {
        assert_eq!(calculate(TESTDATA), 5);
    }
}