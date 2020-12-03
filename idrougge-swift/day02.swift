/* Advent of code 2020, day 2, part 1+2 in Swift 5 */

import Foundation

struct Password: ExpressibleByStringLiteral {
    let range: ClosedRange<Int>
    let letter: Character
    let password: String.SubSequence

    init(stringLiteral line: String) {
        let scanner = Scanner(string: line)
        scanner.charactersToBeSkipped = .none
        let min = scanner.scanInt()!
        _ = scanner.scanCharacter()
        let max = scanner.scanInt()!
        range = min...max
        _ = scanner.scanCharacter()
        letter = scanner.scanCharacter()!
        _ = scanner.scanCharacter()
        _ = scanner.scanCharacter()
        password = line[scanner.currentIndex...]
    }

    var count: Int {
        password.reduce(into: 0) { result, character in
            if character == letter {
                result += 1
            }
        }
    }

    private func character(on offset: Int) -> Character {
        let index = password.index(password.startIndex, offsetBy: offset - 1)
        return password[index]
    }

    var hasRequiredLetter: Bool {
        (character(on: range.lowerBound) == letter) != (character(on: range.upperBound) == letter)
    }
}

let passwords = try String(contentsOfFile: "day02.txt")
    .components(separatedBy: .newlines)
    .map(Password.init)

let part1 = passwords.reduce(into: 0) { count, password in
    if password.range ~= password.count {
        count += 1
    }
}

print(part1)

let part2 = passwords.reduce(into: 0) { count, password in
    if password.hasRequiredLetter {
        count += 1
    }
}

print(part2)
