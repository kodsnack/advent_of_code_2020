use crate::error;

type Data = Vec<i32>;

fn find2(n: i32, v: &[i32]) -> Option<i32> {
    for e in v {
        if v.contains(&(n - e)) {
            return Some(e * (n - e));
        }
    }
    None
}

fn find3(n: i32, v: &[i32]) -> Option<i32> {
    for e in v {
        if let Some(x) = find2(n - e, v) {
            return Some(x * e);
        }
    }
    None
}

pub fn parse(input: &[&str]) -> Result<Data, error::AOCError> {
    input
        .iter()
        .map(|l| {
            l.parse::<i32>()
                .map_err(|_| format!("Couldn't parse {} to int", l).as_str().into())
        })
        .collect()
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
    (
        find2(2020, &v).map(|v| v.to_string()),
        find3(2020, &v).map(|v| v.to_string()),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testhelper::input_from_str;

    const EXAMPLE: &str = "1721
979
366
299
675
1456
";

    #[test]
    fn test_example1() {
        let result = parse(&input_from_str(EXAMPLE)).map(solve).unwrap();
        assert_eq!(result, (Some("514579".into()), Some("241861950".into())))
    }
}
