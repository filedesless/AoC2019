pub fn solve02a(input: &String) -> String {
    let mut initial_state: Vec<usize> = parse(input);
    initial_state[1] = 12;
    initial_state[2] = 2;
    let final_state = run_vm(initial_state);

    final_state[0].to_string()
}

pub fn solve02b(input: &String) -> String {
    for i in 0..100 {
        for j in 0..100 {
            let mut memory: Vec<usize> = parse(input);
            memory[1] = i;
            memory[2] = j;

            memory = run_vm(memory);
            if memory[0] == 19690720 {
                return proof(&memory).to_string();
            }
        }
    }

    String::from("")
}

fn parse(input: &String) -> Vec<usize> {
    input
        .split(',')
        .filter_map(|s| s.parse::<usize>().ok())
        .collect()
}

fn run_vm(mut memory: Vec<usize>) -> Vec<usize> {
    let mut ip = 0;

    loop {
        let op = memory[ip];
        if op == 99 {
            break;
        }

        match (op, memory[ip + 1], memory[ip + 2], memory[ip + 3]) {
            (1, fst, snd, dst) => memory[dst] = memory[fst] + memory[snd],
            (2, fst, snd, dst) => memory[dst] = memory[fst] * memory[snd],
            _ => break,
        }

        ip += 4
    }

    memory
}

fn proof(memory: &Vec<usize>) -> usize {
    100 * memory[1] + memory[2]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vm_test() {
        let expectation = vec![
            ("1,0,0,0,99", "2,0,0,0,99"),
            ("2,3,0,3,99", "2,3,0,6,99"),
            ("2,4,4,5,99,0", "2,4,4,5,99,9801"),
            ("1,1,1,4,99,5,6,0,99", "30,1,1,4,2,5,6,0,99"),
        ];

        for (initial_state, final_state) in expectation {
            assert_eq!(
                run_vm(parse(&String::from(initial_state))),
                parse(&String::from(final_state))
            );
        }
    }

    #[test]
    fn parse_input_test() {
        assert_eq!(parse(&String::from("1,2,3,4")), vec![1, 2, 3, 4]);
    }

    #[test]
    fn proof_test() {
        assert_eq!(proof(&vec![0, 12, 2]), 1202);
    }
}
