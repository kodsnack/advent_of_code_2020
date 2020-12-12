use crate::error;
use crate::vm::*;

type Data = Program;

pub fn parse(input: &[&str]) -> Result<Data, error::AOCError> {
  input
    .iter()
    .map(|l| {
      program_parser(l).map_err(|e| {
        format!("Couldn't parse {}\nerror: {}", l, e)
          .as_str()
          .into()
      })
    })
    .map(|r| r.map(|(_, x)| x))
    .collect()
}

fn until_loop(vm: &mut VM) {
  use std::collections::HashSet;
  let mut executed = HashSet::<usize>::new();
  executed.insert(vm.get_ip());
  while let VMState::Running = vm.step() {
    let ip = vm.get_ip();
    if executed.contains(&ip) {
      return;
    }
    executed.insert(ip);
  }
}

fn solve1(program: &Data) -> Option<String> {
  let mut vm = VM::new(program);
  until_loop(&mut vm);
  Some(format!("{}", vm.get_acc()))
}

fn solve2(program: &Data) -> Option<String> {
  let mut modified_program = program.clone();
  for i in 0.. {
    let old_instr;
    if let Some(instr) = modified_program.get_mut(i) {
      old_instr = *instr;
      match instr {
        Nop(a) => *instr = Jmp(*a),
        Jmp(a) => *instr = Nop(*a),
        _ => continue,
      }
    } else {
      return Some("found no fix".into());
    }
    let mut vm = VM::new(&modified_program);
    until_loop(&mut vm);
    if vm.get_state() == Halted {
      return Some(format!("{}", vm.get_acc()));
    }
    if let Some(instr) = modified_program.get_mut(i) {
      *instr = old_instr;
    } else {
      return Some("this is bad".into());
    }
  }

  Some(format!(""))
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6";

  #[test]
  fn test_example1() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve1(&parsed);
    assert_eq!(result, Some("5".into()))
  }

  #[test]
  fn test_example2() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve2(&parsed);
    assert_eq!(result, Some("8".into()))
  }
}
