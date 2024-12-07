use std::fs;
use std::sync::mpsc::channel;
use std::thread;
use std::time::Instant;

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let paralism = thread::available_parallelism().unwrap().get();
    let mut chunks: Vec<Vec<String>> = Vec::with_capacity(paralism);
    for _ in 0..paralism {
        chunks.push(Vec::new());
    }
    for (idx, line) in puzzle.lines().enumerate() {
        chunks[idx % paralism].push(line.to_string());
    }

    let (sender, receiver) = channel::<(usize, usize)>();

    for chunk in chunks {
        let sender_clone = sender.clone();
        thread::spawn(move || {
            let results = solve_chunk(chunk);
            let _ = sender_clone.send(results);
        });
    }
    drop(sender);

    let mut result_p1 = 0usize;
    let mut result_p2 = 0usize;

    while let Ok((p1, p2)) = receiver.recv() {
        result_p1 += p1;
        result_p2 += p2;
    }

    println!("p1 {}", result_p1);
    println!("p2 {}", result_p1 + result_p2);

    println!("Elapsed: {:.2?}", now.elapsed());
}

fn solve_chunk(chunk: Vec<String>) -> (usize, usize) {
    let mut result_p1 = 0usize;
    let mut result_p2 = 0usize;

    for line in chunk {
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

    (result_p1, result_p2)
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
        let first_nr = numbers[0];
        let solver = Self { numbers, expected };

        if let Some(result) = solver.check_nothing_to_solve() {
            return result;
        }

        solver.solve_recursive(first_nr, 1)
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
    fn solve_recursive(&self, current: usize, offset: usize) -> Result {
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

            if result > self.expected {
                continue;
            }

            if is_last_element {
                if result == self.expected {
                    return if operator == 2 {
                        Result::SolvedWithCombinator
                    } else {
                        Result::Solved
                    };
                }
                continue;
            }

            match self.solve_recursive(result, offset + 1) {
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
