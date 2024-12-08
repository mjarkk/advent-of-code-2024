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

enum Result {
    Solved,
    SolvedWithCombinator,
    NotSolved,
}

struct Solver {
    numbers: Vec<usize>,
    expected: usize,
}

impl Solver {
    fn solve(expected: usize, numbers: Vec<usize>) -> Result {
        // let first_nr = numbers[0];
        let solver = Self { numbers, expected };

        if let Some(result) = solver.check_nothing_to_solve() {
            return result;
        }

        match solver.solve_recursive_reverse(expected, solver.numbers.len() - 1, false) {
            Result::Solved => return Result::Solved,
            Result::SolvedWithCombinator => panic!("wut? this should not happen"),
            Result::NotSolved => {
                // try the same with the combine operator
            }
        };

        solver.solve_recursive_reverse(expected, solver.numbers.len() - 1, true)
    }
    fn check_nothing_to_solve(&self) -> Option<Result> {
        if self.numbers.len() != 1 {
            None
        } else if self.numbers[0] == self.expected {
            Some(Result::Solved)
        } else {
            Some(Result::NotSolved)
        }
    }
    fn solve_recursive_reverse(
        &self,
        current_answer: usize,
        offset: usize,
        use_combine_operator: bool,
    ) -> Result {
        let nr = self.numbers[offset];

        if offset == 0 {
            return if current_answer == nr {
                Result::Solved
            } else {
                Result::NotSolved
            };
        }

        for operator in 0..(if use_combine_operator { 3 } else { 2 }) {
            let new_answer = match operator {
                0 => {
                    if nr > current_answer {
                        continue;
                    }
                    current_answer - nr
                }
                1 => {
                    if nr > current_answer || current_answer % nr != 0 {
                        continue;
                    }
                    current_answer / nr
                }
                2 => match cut_number_from_end(nr, current_answer) {
                    Some(answer) => answer,
                    None => continue,
                },
                _ => panic!("Unknown operator"),
            };

            match self.solve_recursive_reverse(new_answer, offset - 1, use_combine_operator) {
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

fn cut_number_from_end(ends_with: usize, nr: usize) -> Option<usize> {
    let mut exp = 1u32;
    let mut temp = ends_with;
    loop {
        if temp < 10 {
            break;
        }

        temp = temp / 10;
        exp += 1;
    }

    let power = usize::pow(10, exp);
    if (nr % power) != ends_with {
        return None;
    }

    let new_nr = (nr - ends_with) / power;
    if new_nr == 0 {
        return None;
    }

    Some(new_nr)
}
