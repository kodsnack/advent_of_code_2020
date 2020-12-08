/* Advent of code 2020, day 8, part 1+2 in Swift 5 */

import Foundation

let input = try! String(contentsOfFile: "day08.txt")
    .components(separatedBy: "\n")
    .map(Instruction.init)

enum Instruction: ExpressibleByStringLiteral {
    case acc(Int), jmp(Int), nop(Int)

    init(stringLiteral value: String) {
        let scanner = Scanner(string: value)
        let opcode = scanner.scanUpToCharacters(from: .whitespaces)!
        let operand = scanner.scanInt()!
        switch opcode {
        case "acc": self = .acc(operand)
        case "jmp": self = .jmp(operand)
        case "nop": self = .nop(operand)
        case _: fatalError(opcode)
        }
    }

    var flipped: Self {
        switch self {
        case .nop(let n): return .jmp(n)
        case .jmp(let n): return .nop(n)
        case _: return self
        }
    }
}

struct CPU {
    var pc: Int = 0
    var accumulator: Int = 0
    var seen: Set<Int> = []

    mutating func run(_ program: [Instruction]) {
        while pc < program.endIndex {
            let instruction = program[pc]
            guard seen.insert(pc).inserted
            else { return }
            switch instruction {
            case .acc(let value): accumulator += value
            case .jmp(let offset): pc += offset - 1
            case .nop: break
            }
            pc += 1
        }
    }
}

var cpu = CPU()
cpu.run(input)
print(cpu.accumulator)

for i in input.indices {
    var cpu = CPU()
    var program = input
    let instruction = program[i]
    if case .acc = instruction { continue }
    program[i] = instruction.flipped
    cpu.run(program)
    if cpu.pc == program.endIndex {
        print(cpu.accumulator)
        break
    }
}
