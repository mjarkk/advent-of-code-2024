use std::fs;
use std::time::Instant;

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut result_p1 = 0usize;
    let mut result_p2 = 0usize;

    for line in puzzle.lines() {
        if line.is_empty() {
            continue;
        }

        let mut result = 0usize;
        let mut numbers: Vec<usize> = Vec::new();
        for (idx, part) in line.split(" ").enumerate() {
            if idx == 0 {
                result = part[0..part.len() - 1].parse().unwrap();
                continue;
            }

            let number: usize = part.parse().unwrap();
            numbers.push(number);
        }

        match Solver::solve(result, numbers) {
            Result::Solved => result_p1 += result,
            Result::SolvedWithCombinator => result_p2 += result,
            Result::NotSolved => { /* Do nothing */ }
        }
    }

    println!("p1 {}", result_p1);
    println!("p2 {}", result_p1 + result_p2);

    println!("Elapsed: {:.2?}", now.elapsed());
}

fn check_nothing_to_solve(expected_result: usize, numbers: &Vec<usize>) -> Option<Result> {
    match numbers.len() {
        0 => Some(Result::NotSolved),
        1 => Some(if numbers[0] == expected_result {
            Result::Solved
        } else {
            Result::NotSolved
        }),
        _ => None,
    }
}

enum Result {
    Solved,
    SolvedWithCombinator,
    NotSolved,
}

struct Solver {
    numbers: Vec<usize>,
}

impl Solver {
    fn solve(expected: usize, numbers: Vec<usize>) -> Result {
        if let Some(result) = check_nothing_to_solve(expected, &numbers) {
            return result;
        }

        let first_nr = numbers[0];
        let solver = Self { numbers };

        solver.solve_recursive(expected, first_nr, 1)
    }
    fn solve_recursive(&self, expected: usize, current: usize, offset: usize) -> Result {
        let next = self.numbers[offset];
        let is_last_element = offset == self.numbers.len() - 1;

        for operator in 0..3 {
            let result = match operator {
                0 => current + next,
                1 => current * next,
                2 => {
                    let mut exp = 1u32;
                    let mut temp = next;
                    loop {
                        if temp < 10 {
                            break;
                        }

                        temp = temp / 10;
                        exp += 1;
                    }

                    current * usize::pow(10, exp) + next
                }
                _ => panic!("Unknown operator"),
            };

            if result > expected {
                continue;
            }

            if is_last_element {
                if result == expected {
                    return if operator == 2 {
                        Result::SolvedWithCombinator
                    } else {
                        Result::Solved
                    };
                }
                continue;
            }

            match self.solve_recursive(expected, result, offset + 1) {
                Result::Solved => {
                    return if operator == 2 {
                        Result::SolvedWithCombinator
                    } else {
                        Result::Solved
                    };
                }
                Result::SolvedWithCombinator => return Result::SolvedWithCombinator,
                Result::NotSolved => { /* Do nothing */ }
            }
        }

        Result::NotSolved
    }
}
