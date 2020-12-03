use std::io::Write;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

pub fn color_choice(choice: &str) -> ColorChoice {
    match choice {
        "auto" => ColorChoice::Auto,
        "always" => ColorChoice::Always,
        "ansi" => ColorChoice::AlwaysAnsi,
        _ => ColorChoice::Never,
    }
}

pub fn write(stream: &mut StandardStream, content: &str, color: Option<Color>) {
    stream.set_color(ColorSpec::new().set_fg(color)).unwrap();
    write!(stream, "{}", content).unwrap();
}
