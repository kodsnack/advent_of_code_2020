use crate::AOCError;
use crate::AOCResult;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

pub fn get_input_from_file(day: u32) -> AOCResult<Vec<String>> {
    let filename = format!("inputs/day{}.txt", day);
    let path = Path::new(filename.as_str());

    let mut file: File = File::open(&path).map_err::<AOCError, _>(|e| {
        format!("cause: {}, filename: {}", e, filename)
            .as_str()
            .into()
    })?;
    let mut content = String::new();
    file.read_to_string(&mut content)
        .map_err::<AOCError, _>(|e| {
            format!("cause: {}, filename: {}", e, filename)
                .as_str()
                .into()
        })?;
    Ok(content.lines().map(String::from).collect())
}
