use std::collections::BTreeMap;
use std::fs::File;
use std::io::Read;
use std::path::Path;

pub mod day01;
pub mod day02;

pub fn get_input(challenge: u32) -> String {
    let s = format!("input/{:02}.txt", challenge);
    let path = Path::new(&s);

    let mut file = match File::open(&path) {
        Err(_) => panic!("Couldn't open {}", s),
        Ok(file) => file,
    };

    let mut s = String::new();
    match file.read_to_string(&mut s) {
        Err(_) => panic!("Couldn't read content from {}", s),
        Ok(_) => s,
    }
}

pub fn get_solvers() -> BTreeMap<u32, [fn(&String) -> String; 2]> {
    let mut solvers = BTreeMap::new();
    solvers.insert(1, [day01::solve01a, day01::solve01b]);
    solvers.insert(2, [day02::solve02a, day02::solve02b]);
    solvers
}

#[cfg(test)]
mod solved_tests {
    use super::*;

    #[test]
    fn verify_answers() {
        let answers = [
            (1, [String::from("3331849"), String::from("4994898")]),
            (2, [String::from("5110675"), String::from("4847")]),
        ];
        let solvers = get_solvers();
        for (day, expectations) in &answers {
            let content = get_input(*day);
            if let Some(daily_solvers) = solvers.get(day) {
                for (solver, expectation) in daily_solvers.iter().zip(expectations) {
                    assert_eq!(&solver(&content), expectation);
                }
            } else {
                panic!("Missing solution for day {:02}", day);
            }
        }
    }
}
