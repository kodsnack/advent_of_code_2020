use std::collections::HashSet;

use std::convert::TryFrom;

// You narrow the problem down to a strange infinite loop in the boot code (your puzzle input) of
// the device. You should be able to fix it, but first you need to be able to run the code in isolation.
//
// The boot code is represented as a text file with one instruction per line of text.
//  Each instruction consists of an operation (acc, jmp, or nop) and an argument (a signed number like +4 or -20).
//
//     acc increases or decreases a single global value called the accumulator by the value given in
//         the argument. For example, acc +7 would increase the accumulator by 7.
//         The accumulator starts at 0. After an acc instruction, the instruction immediately below
//         it is executed next.
//     jmp jumps to a new instruction relative to itself. The next instruction to execute is found
//         using the argument as an offset from the jmp instruction; for example, jmp +2 would skip the next instruction, jmp +1 would continue to the instruction immediately below it, and jmp -20 would cause the instruction 20 lines above to be executed next.
//     nop stands for No OPeration - it does nothing. The instruction immediately below it is
//         executed next.
//
// For example, consider the following program:
//
// nop +0
// acc +1
// jmp +4
// acc +3
// jmp -3
// acc -99
// acc +1
// jmp -4
// acc +6
//
// These instructions are visited in this order:
//
// nop +0  | 1
// acc +1  | 2, 8(!)
// jmp +4  | 3
// acc +3  | 6
// jmp -3  | 7
// acc -99 |
// acc +1  | 4
// jmp -4  | 5
// acc +6  |
//
// First, the nop +0 does nothing. Then, the accumulator is increased from 0 to 1 (acc +1) and
// jmp +4 sets the next instruction to the other acc +1 near the bottom. After it increases the
// accumulator from 1 to 2, jmp -4 executes, setting the next instruction to the only acc +3. It
// sets the accumulator to 5, and jmp -3 causes the program to continue back at the first acc +1.
//
// This is an infinite loop: with this sequence of jumps, the program will run forever. The moment
// the program tries to run any instruction a second time, you know it will never terminate.
//
// Immediately before the program would run an instruction a second time, the value in the
// accumulator is 5.
//
// Run your copy of the boot code. Immediately before any instruction is executed a second time,
// what value is in the accumulator?

pub fn first(input: String) -> String {
    let instr: Vec<Instruction> = input
        .lines()
        .filter_map(|line| Instruction::from(line).ok())
        .collect::<Vec<Instruction>>();

    let mut executed_instr: HashSet<usize> = HashSet::with_capacity(instr.len());

    let mut instr_ptr: ArchSize = 0;
    let mut acc: ArchSize = 0;
    loop {
        let safe_ptr: usize = usize::try_from(instr_ptr).expect("Overflow on pointer conversion");
        if executed_instr.contains(&safe_ptr) {
            return acc.to_string();
        }

        let next_instr: &Instruction = instr
            .get(safe_ptr)
            .expect(&format!("Failed to get instruction at index {}", instr_ptr));

        executed_instr.insert(safe_ptr);

        match next_instr {
            Instruction::Nop(_) => instr_ptr += 1,
            Instruction::Jmp(m) => instr_ptr += m,
            Instruction::Acc(v) => {
                instr_ptr += 1;
                acc += v;
            }
        }
    }
}

pub fn second(input: String) -> String {
    let instr: Vec<Instruction> = input
        .lines()
        .filter_map(|line| Instruction::from(line).ok())
        .collect::<Vec<Instruction>>();

    let halt_and_catch_fire: usize = instr.len();
    let mut comp = Computer::load(instr.clone());
    let mut restart_attempts = 0;
    let mut executed_instr: usize = 0;
    loop {
        match comp.next() {
            Some((i, _)) if i == halt_and_catch_fire => break,
            None => break,
            _ => {
                if executed_instr < halt_and_catch_fire {
                    executed_instr += 1;
                } else {
                    if restart_attempts == halt_and_catch_fire {
                        panic!("ffs")
                    }
                    comp.restart();
                    println!("Flipping {}", restart_attempts);
                    comp.flip(restart_attempts);
                    executed_instr = 0;
                    restart_attempts += 1;
                }
            }
        }
    }
    return comp.acc().to_string();
}

type ArchSize = isize;

struct Computer {
    firmware: Vec<Instruction>,
    instructions: Vec<Instruction>,
    ptr: usize,
    acc: isize,
}

impl Computer {
    pub fn load(instructions: Vec<Instruction>) -> Computer {
        Computer {
            firmware: instructions.clone(),
            instructions,
            ptr: 0,
            acc: 0,
        }
    }

    pub fn acc(&self) -> isize {
        self.acc
    }

    pub fn restart(&mut self) {
        self.ptr = 0;
        self.acc = 0;
        self.instructions = self.firmware.clone();
    }

    pub fn flip(&mut self, i: usize) {
        let new_instr: Instruction = match self.instructions.remove(i) {
            Instruction::Jmp(n) => Instruction::Nop(n),
            Instruction::Nop(n) => Instruction::Jmp(n),
            x => x,
        };
        self.instructions.insert(i, new_instr)
    }

    fn move_pointer(&mut self, change: isize) {
        let abs_change: usize = change.abs() as usize;
        if change < 0 {
            self.ptr = self
                .ptr
                .checked_sub(abs_change)
                .expect("Unable to subtract");
        } else {
            self.ptr = self.ptr.checked_add(abs_change).expect("Unable to add");
        }
    }
}

impl Iterator for Computer {
    type Item = (usize, Instruction);

    fn next(&mut self) -> Option<(usize, Instruction)> {
        let ptr_before: usize = self.ptr;
        let next: Instruction = self.instructions.get(self.ptr)?.clone();
        match next {
            Instruction::Nop(_) => self.move_pointer(1),
            Instruction::Jmp(n) => self.move_pointer(n),
            Instruction::Acc(n) => {
                self.move_pointer(1);
                self.acc += n;
            }
        };
        Some((ptr_before, next))
    }
}

#[derive(Debug, Copy, Clone)]
enum Instruction {
    Nop(ArchSize),
    Jmp(ArchSize),
    Acc(ArchSize),
}

impl Instruction {
    fn from(input: &str) -> Result<Instruction, InstrErr> {
        let parts = input.trim().split_ascii_whitespace().collect::<Vec<&str>>();
        let inst: &str = parts.get(0).ok_or(InstrErr::NoInstruction)?;
        let num: ArchSize = parts
            .get(1)
            .ok_or(InstrErr::NoNum)?
            .parse::<ArchSize>()
            .map_err(|_| InstrErr::NoNum)?;
        match inst {
            "nop" => Ok(Instruction::Nop(num)),
            "acc" => Ok(Instruction::Acc(num)),
            "jmp" => Ok(Instruction::Jmp(num)),
            _ => Err(InstrErr::UnsupportedInstruction(format!(
                "Unsupported instructiuon: {}",
                inst
            ))),
        }
    }
}

enum InstrErr {
    NoInstruction,
    NoNum,
    UnsupportedInstruction(String),
}

#[cfg(test)]
mod tests {
    use super::second;

    #[test]
    fn test_part2_break_out() {
        let input = r"
            nop +0
            acc +1
            jmp +4
            acc +3
            jmp -3
            acc -99
            acc +1
            jmp -4
            acc +6
            ";
        second(input.to_string());
    }
}
