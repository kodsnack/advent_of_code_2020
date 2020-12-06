/* Advent of code 2020, day 6, part 1+2 in Swift 5 */

import Foundation

let filename = "day06.txt"
let input = try! String(contentsOfFile: filename)
    .components(separatedBy: "\n\n")
    .map { $0.components(separatedBy: "\n") }

let part_1 = input
    .map { $0.flatMap(Array.init) }
    .map(Set.init)
    .map(\.count)
    .reduce(0, +)
print(part_1)

let part_2 = input
   .map { $0.map(Set.init) }
   .flatMap { sets in
       sets.reduce(sets.first!) { common, part in
           common.intersection(part)
       }
   }
   .map(\.count)
   .reduce(0, +)
print(part_2)
