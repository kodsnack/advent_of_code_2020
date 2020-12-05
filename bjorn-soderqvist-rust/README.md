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

