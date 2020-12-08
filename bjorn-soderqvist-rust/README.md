# advent-of-code-2020

Watch and laugh as I try out Rust for the first time.


## Today I learned...

These are some things I learned along the way.


### Day 1

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


# Day 7

- First time using a `tuple`.

I was considering using something clever for the text parsing:
- `nom` seems to be popular, however I deemed myself too impatient to try and learn it.
- I don't like regular expressions.
- In the end I just went with string slices, `lines()`, `split()`, `replace()`

At the end of the solution I just kept trying to fix whatever the compiler told me was wrong instead of thinking. There's a lot of things I am not so certain about, when should I `clone` a string and when should I lend it out? 


# Day 8

I was quite pleased with my first attempt at making a `run()` function returning a `Result<i32,  &'static str>`. Then threw it out because of the following: I wanted a function to mutate the vector that was passed as an argument. But since the vector is borrowed that's not allowed. I tried various approaches and in the end I just gave up and made a big function containing everything.

In part 2 I'm starting to understand a bit more about how to control flow in Rust. Even though I would have liked to break it up into more functions, I like how there's an outer `for` loop which controls which line to alter, then a `loop` for the actual program and it will `break` as soon as it has detected that the virtual program is stuck in a loop (or the next line is a negative number). Only if it correctly detects a solution it will return a number. If it can't find any solutions it will `panic`.

