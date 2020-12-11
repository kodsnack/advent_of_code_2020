use crate::parser;
use crate::parser::IResult;

#[derive(Debug, Clone, Copy)]
pub enum Operation {
    Acc,
    Jmp,
    Nop,
}
pub use Operation::*;

#[derive(Debug, Clone, Copy)]
pub struct Instruction {
    pub op: Operation,
    pub a: i64,
}

pub type Program = Vec<Instruction>;

#[derive(Debug, Clone)]
pub struct VM<'a> {
    ip: usize,
    acc: i64,
    program: &'a Program,
    state: VMState,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VMState {
    Running,
    Halted,
    VMErr(&'static str),
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
    let instr = vm.program.get(vm.ip)?;
    Some(*instr)
}

fn jmp(vm: &mut VM, instr: &Instruction) {
    let newip = vm.ip as i64 + instr.a;
    if newip < 0 {
        vm.state = VMErr("negative ip");
    }
    vm.ip = newip as usize;
}

fn acc(vm: &mut VM, instr: &Instruction) {
    vm.acc += instr.a;
    vm.ip += 1;
}

fn nop(vm: &mut VM, _instr: &Instruction) {
    vm.ip += 1;
}

fn step(vm: &mut VM) {
    match current_instr(vm) {
        Some(instr) => {
            match instr.op {
                Jmp => jmp(vm, &instr),
                Acc => acc(vm, &instr),
                Nop => nop(vm, &instr),
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
    let operation = alt((
        map(tag("jmp "), |_| Jmp),
        map(tag("acc "), |_| Acc),
        map(tag("nop "), |_| Nop),
    ));
    let operand = terminated(
        map_res(
            recognize(tuple((
                opt(alt((char('-'), char('+')))),
                is_a("0123456789"),
            ))),
            |x: &str| x.parse::<i64>(),
        ),
        space0,
    );
    let mut instruction = map(tuple((operation, operand)), |(op, a)| Instruction { op, a });
    instruction(input)
}
