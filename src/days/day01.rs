use std::cmp::max;

pub fn solve01a(input: &String) -> String {
    String::from(compute(input, false).to_string())
}

pub fn solve01b(input: &String) -> String {
    String::from(compute(input, true).to_string())
}

fn compute(input: &String, account_for_fuel: bool) -> i32 {
    input
        .lines()
        .filter_map(|line| line.parse::<i32>().ok())
        .map(|i| compute_fuel(i, account_for_fuel))
        .sum()
}

fn compute_fuel(mass: i32, account_for_fuel: bool) -> i32 {
    let mut res = mass / 3 - 2;

    if account_for_fuel && res >= 0 {
        res += compute_fuel(res, true);
    }

    max(res, 0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn division_is_floored() {
        assert_eq!(12 / 3, 4);
        assert_eq!(14 / 3, 4);
    }

    #[test]
    fn compute_fuel_test() {
        let expectation = vec![
            (12, 2, false),
            (14, 2, false),
            (1969, 654, false),
            (100756, 33583, false),
            (12, 2, true),
            (1969, 966, true),
            (100756, 50346, true),
        ];

        for (mass, fuel, account) in expectation {
            assert_eq!(compute_fuel(mass, account), fuel);
        }
    }
}
