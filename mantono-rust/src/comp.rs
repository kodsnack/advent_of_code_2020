use std::{fmt::Debug, str::FromStr};

trait ArchSize: Sized + Copy + Clone + FromStr + Debug {}
impl<T> ArchSize for T where T: Sized + Copy + Clone + FromStr + Debug {}

#[derive(Debug, Copy, Clone)]
enum Instruction<T: ArchSize> {
    Nop(T),
    Jmp(T),
    Acc(T),
}

impl<T: ArchSize> Instruction<T> {
    fn from(input: &str) -> Result<Instruction<T>, InstrErr> {
        let parts = input.trim().split_ascii_whitespace().collect::<Vec<&str>>();
        let inst: &str = parts.get(0).ok_or(InstrErr::NoInstruction)?;
        let num: T = parts
            .get(1)
            .ok_or(InstrErr::NoNum)?
            .parse::<T>()
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

type ArchType = isize;
enum InstrErr {
    NoInstruction,
    NoNum,
    UnsupportedInstruction(String),
}

struct Computer {
    instructions: Vec<Instruction<ArchType>>,
    ptr: usize,
    acc: isize,
}

impl Computer {
    pub fn load(instructions: Vec<Instruction<ArchType>>) -> Computer {
        Computer {
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
    }

    fn move_ptr(&mut self, change: isize) {
        let abs_change: usize = change.abs() as usize;
        if change < 0 {
            self.ptr = self
                .ptr
                .checked_sub(abs_change)
                .expect("Unable to subtract");
        } else {
            self.umove_ptr(abs_change)
        }
    }

    fn umove_ptr(&mut self, change: usize) {
        self.ptr = self.ptr.checked_add(change).expect("Overflow on add");
    }
}

impl Iterator for Computer {
    type Item = (usize, Instruction<ArchType>);

    fn next(&mut self) -> Option<(usize, Instruction<ArchType>)> {
        let ptr_before: usize = self.ptr;
        let next: Instruction<ArchType> = self.instructions.get(self.ptr)?.clone();
        match next {
            Instruction::Nop(_) => self.umove_ptr(1),
            Instruction::Jmp(n) => self.move_ptr(n),
            Instruction::Acc(n) => {
                self.umove_ptr(1);
                self.acc += n as isize;
            }
        };
        Some((ptr_before, next))
    }
}
