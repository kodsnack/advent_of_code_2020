# advent-of-code-2020

Watch and laugh as I try out Rust for the first time.


# Notes ("Today I learned")

## Day 1

- You don't have to have a local dev environment, you can begin coding in the browser: [https://play.rust-lang.org](https://play.rust-lang.org)
- It's preferred to avoid parens in the if clause: `if 1+1 == 2 {...}`


## Day 2

- There's a lot of ways you can read a file.
- I sprinkled a lot of `.unwrap()` in my code. It's because of something called `Option`.
- There's `String` and there's `str`.
- Rust has a compact template type notation, for instance `<_'>`. Hard to read as a newbie but the documentation site is pretty good.
- I generally find the compiler errors to be very helpful. As a complete beginner I try something out, and quickly find out whether the syntax was OK, if there's something wrong with the types, or something missing.


## Day 3

- There's ever more ways you can read a file.


## Day 4

- I knew there's many features of the language I've avoided until now.
- Tried out `struct`. Easy enough so far.
- Tried out `match`. I really like it and wish there was something similar in JavaScript.
- Tried out some implicit returns. Visibly it's a useful distinction that it mustn't end with a semicolon.
- I started using `cargo` to manage the following:
- Added a "workspace" and migrated all days into "packages". Now I get all builds into the same folder. Neat.
- From file-reading-fatigue I didn't look up a more practical way to read the file. I ended up doing an ugly workaround in the code instead.


## Day 5

- To run just the package from day 5, use [`cargo run -p day5`](https://doc.rust-lang.org/cargo/commands/cargo-run.html).
- You can write the unit tests at the bottom of the same file. Actually a pretty nice feature!
- To get automatic feedback after each save, you can [`cargo install cargo-watch`](https://crates.io/crates/cargo-watch).
- To import a file as a `utf-8` string, use the macro [`include_str!("input.txt")`](https://doc.rust-lang.org/std/macro.include_str.html).


## Day 6

- I got to try out both `HashSet` and `HashMap` today
- Being most proficient in JavaScript I have no understanding of where I need `&`, `&&`, `*` or `**`. Rustc is extremely forgiving and keeps correcting me until the code seems good. After which point it works. Magic?
- `hashmap.entry(key)or_insert(0) += 1` was perfect for a histogram scenario. In JS I'd need to check for presence of the key and optionally setting it to 0 before adding 1 to it.


## Day 7

- First time using a `tuple`.

I was considering using something clever for the text parsing:
- `nom` seems to be popular, however I deemed myself too impatient to try and learn it.
- I don't like regular expressions.
- In the end I just went with string slices, `lines()`, `split()`, `replace()`

At the end of the solution I just kept trying to fix whatever the compiler told me was wrong instead of thinking. There's a lot of things I am not so certain about, when should I `clone` a string and when should I lend it out? 


## Day 8

I was quite pleased with my first attempt at making a `run()` function returning a `Result<i32,  &'static str>`. Then threw it out because of the following: I wanted a function to mutate the vector that was passed as an argument. But since the vector is borrowed that's not allowed. I tried various approaches and in the end I just gave up and made a big function containing everything.

In part 2 I'm starting to understand a bit more about how to control flow in Rust. Even though I would have liked to break it up into more functions, I like how there's an outer `for` loop which controls which line to alter, then a `loop` for the actual program and it will `break` as soon as it has detected that the virtual program is stuck in a loop (or the next line is a negative number). Only if it correctly detects a solution it will return a number. If it can't find any solutions it will `panic`.


## Day 9

- First time I got to use Option<T> :) I think that it fit quite well for this use case. A function which finds, or doesn't find something. 
- Today I installed the Rust extension for VS Code. It includes the Rust Language Server. Pretty nice to get this level of support immediately in the editor. The only problem is that it keeps running and building all the time - the computer gets warm after just a few minutes. I might try switching to the other language server "rust-analyzer" tomorrow.
- Trying out [TabNine autocomplete](https://marketplace.visualstudio.com/items?itemName=TabNine.tabnine-vscode) which seems to be a pretty good autocomplete! It's not just for Rust.


## Day 10

- I tried out a nice feature, `std::slice::Windows` which works great. But it looked like the wrong tool for this solution (because of the wall also being connected to the first plug). To make it work I had to add 4 ugly lines of code.
- Improved the aforementioned wall plug problem by `insert`ing a 0 in the start of the vector.
- Finally understood how to share a lib file between my part1 and part2 files: `use super::lib;`
- In my attempts at learning more Rust I didn't just settle for using a normal `usize` to represent each power plug. I implemented a `pub struct Adapter` which I then began working on:
- I implemented subtraction for this Struct so that I can map out the difference between two plugs: `impl Sub for &Adapter`
- I implemented Display for this Struct so that I can print just the number to the console: `impl fmt::Display for Adapter`
- I derived `PartialEq, Eq, PartialOrd, Ord` for this Struct so that it can be sorted
- I derived `Debug` because `assert_eq!` needs this

With regards to problem solving the second part, I found the brute force solution quite quickly. I know that I can split the data into chunks every time that there's a gap of 3 - but I kind of lost the energy. Might revisit this later.


## Day 11

Yesterday I liked testing out the `trait`s and today I really could see their potential power.

- You can implement a `trait` for `Display`. So the flight layout can be printed to the console in a beautiful way. The consumer doesn't need to know how, it's only a matter of `println!("{}", flight);`. The more complex your data structure the more useful this is.
- You can implement `trait`s for equality comparison. So the consumer can write `if flight1 == flight2` and you have put the implementation of the deep comparison in your lib with the struct itself.
- I'm not sure if `usize` is the right tool for these positions, since I constantly have to avoid going to -1 in any axis.

For Rust I feel like it takes longer to test ideas when you're uncertain about the solution. When I've got a clearly defined problem (and that I've clearly understood) it's only a matter of taking small steps along TDD. Advent of Code is very well suited for this, since you get example data for each problem. Truly; when I had done the prep work of the `Struct Flight`, its logging formatter `impl fmt::Display for Flight`, its comparison traits `impl PartialEq for Flight` and `impl Eq for Flight {}` it was only a matter of writing unit tests and implementing the corresponding functions. My feeling is that once the tests based on the example data go through, it's very probable that the code works also for the input data from the site. 

As a JavaScript / Typescript developer it's very different from how I normally work. Once I've written and saved a few lines of JS, there's no guarantee that this code will actually do anything meaningful in the browser. Most of the time it's not. For Typescript this is different in that I'll spend more time in the authoring phase in the editor until I can see that the types make sense, before I need to refresh the browser.
This is kind of how it looks to me:

### Time spent

|            | Syntax and types | Checking whether "it works" |
|------------|------------------|-----------------------------|
| JavaScript | 1%               | 99%                         |
| Typescript | 25%              | 75%                         |
| Rust       | 99%              | 1%                          |


One thing I want to try out is the use of `&self`, I feel like that would have been very natural today (if it works the way I assume). Now I keep passing `flight: &Flight` to all functions.


## Day 12

- I realized how to avoid the VSCode Rust plugin building at 100% all of the time: In the workspace `Cargo.toml` I comment away all other days than today. That way it doesn't keep building the other ones. The root problem seems to be that this watcher process actually doesn't stop building and wait for changes. Instead it just keeps building in an eternal loop. I wonder if there's a simple fix to that that I'm just not aware of.
- I miss the way that I can express a type in Typescript which is a union of several values like `type animal = "cat" | "dog"`. Now I've instead done a `match` where it catches any invalid inputs:
```rust
match instruction.direction {
    'N' => y -= instruction.steps,
    'S' => y += instruction.steps,
    'E' => x += instruction.steps,
    'W' => x -= instruction.steps,
    _ => panic!("Unknown direction {}", instruction.direction),
}
```

This day's Part 1 was a little easier than the last days, at least for me. I wonder if it's also because I've become more proficient in Rust?

