use crate::parser;
use crate::parser::IResult;

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    Acc(i64),
    Jmp(i64),
    Nop(i64),
}
pub use Instruction::*;

pub type Program = Vec<Instruction>;

#[derive(Debug, Clone)]
pub struct VM<'a> {
    ip: usize,
    acc: i64,
    program: &'a Program,
    state: VMState<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VMState<'a> {
    Running,
    Halted,
    VMErr(&'a str),
}
pub use VMState::*;

impl VM<'_> {
    pub fn new(program: &Program) -> VM {
        VM {
            acc: 0,
            ip: 0,
            program,
            state: Running,
        }
    }
    pub fn step(&mut self) -> VMState {
        step(self);
        self.state
    }
    pub fn get_state(&self) -> VMState {
        self.state
    }
    pub fn get_ip(&self) -> usize {
        self.ip
    }
    pub fn get_acc(&self) -> i64 {
        self.acc
    }
}

fn current_instr(vm: &VM) -> Option<Instruction> {
    vm.program.get(vm.ip).copied()
}

fn step(vm: &mut VM) {
    match current_instr(vm) {
        Some(instr) => {
            vm.ip += 1;
            match instr {
                Jmp(a) => {
                    let newip = vm.ip as i64 + a - 1;
                    if newip < 0 {
                        vm.state = VMErr("negative ip");
                    }
                    vm.ip = newip as usize;
                }
                Acc(a) => vm.acc += a,
                Nop(_) => (),
            };
            vm.state = Running
        }
        None => {
            if vm.ip == vm.program.len() {
                vm.state = Halted
            } else {
                vm.state = VMErr("ip out of bounds")
            }
        }
    }
}

pub fn program_parser(input: &str) -> IResult<&str, Instruction> {
    use parser::*;
    fn operand(input: &str) -> IResult<&str, i64> {
        terminated(
            map_res(
                recognize(tuple((
                    opt(alt((char('-'), char('+')))),
                    is_a("0123456789"),
                ))),
                |x: &str| x.parse::<i64>(),
            ),
            space0,
        )(input)
    }
    let mut instruction = alt((
        map(preceded(tag("jmp "), operand), Jmp),
        map(preceded(tag("acc "), operand), Acc),
        map(preceded(tag("nop "), operand), Nop),
    ));
    instruction(input)
}
