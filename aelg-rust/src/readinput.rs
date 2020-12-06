use crate::AOCError;
use crate::AOCResult;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn read_file(path: &Path) -> AOCResult<String> {
    let mut file: File = File::open(&path).map_err::<AOCError, _>(|e| {
        format!("cause: {}, filename: {}", e, path.display())
            .as_str()
            .into()
    })?;
    let mut content = String::new();
    file.read_to_string(&mut content)
        .map_err::<AOCError, _>(|e| {
            format!("cause: {}, filename: {}", e, path.display())
                .as_str()
                .into()
        })?;
    Ok(content)
}

pub fn get_input_from_file(day: u32) -> AOCResult<Vec<String>> {
    download_input(day)?;

    let filename = format!("inputs/day{}.txt", day);
    let path = Path::new(filename.as_str());

    let content = read_file(path)?;
    Ok(content.lines().map(String::from).collect())
}

pub fn download_input(day: u32) -> AOCResult<()> {
    let filename = format!("inputs/day{}.txt", day);
    let path = Path::new(filename.as_str());

    if path.exists() {
        // Do not down if exists.
        return Ok(());
    }

    let auth_file = "auth.txt";
    let auth_string = read_file(Path::new(auth_file)).map_err::<AOCError, _>(|e| {
        format!(
                // Maybe not the best place to put this...
                "To enable input download put your session cookie value in auth.txt\nError: {}",
                e
            )
        .as_str()
        .into()
    })?;

    let uri = format!("https://adventofcode.com/2020/day/{}/input", day);
    let cookie_value = format!("session={}", auth_string.trim());

    let response = ureq::get(uri.as_str())
        .set("Cookie", cookie_value.as_str())
        .call();
    if !response.ok() {
        return Err(format!(
            "Download failed, status: {}\n{}",
            response.status(),
            response.into_string().unwrap_or_default()
        )
        .as_str()
        .into());
    }
    let content = response
        .into_string()
        .map_err::<AOCError, _>(|e| format!("request failed: {}", e).as_str().into())?;

    let mut file: File = File::create(&path).map_err::<AOCError, _>(|e| {
        format!("cause: {}, filename: {}", e, filename)
            .as_str()
            .into()
    })?;

    file.write_all(content.as_bytes())
        .map_err::<AOCError, _>(|e| {
            format!("Couldn't write downloaded input cause: {}", e)
                .as_str()
                .into()
        })?;

    Ok(())
}
