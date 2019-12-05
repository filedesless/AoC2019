mod days;

fn main() {
    let solvers = days::get_solvers();

    for (day, daily_solvers) in solvers {
        let content = days::get_input(day);
        for (problem_number, solver) in (1..).zip(&daily_solvers) {
            println!(
                "Solution for day {:02} (#{}) is: {}",
                day,
                problem_number,
                solver(&content)
            );
        }
    }
}
