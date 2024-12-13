use std::cmp::{max, min};
use std::fs;
use std::time::Instant;

struct Puzzle {
    a: (usize, usize),
    b: (usize, usize),
    answer: (usize, usize),
}

impl Default for Puzzle {
    fn default() -> Self {
        Puzzle {
            a: (0, 0),
            b: (0, 0),
            answer: (0, 0),
        }
    }
}

fn main() {
    let now = Instant::now();
    let input = fs::read_to_string("./puzzle.txt").unwrap();

    let mut puzzles: Vec<Puzzle> = Vec::new();
    for block in input.split("\n\n") {
        let mut puzzle = Puzzle::default();
        let mut has_answer = false;
        for (idx, line) in block.lines().enumerate() {
            if idx < 2 {
                let x = &line[12..14];
                let y = &line[18..20];
                let cords: (usize, usize) = (x.parse().unwrap(), y.parse().unwrap());
                if idx == 0 {
                    puzzle.a = cords;
                } else {
                    puzzle.b = cords;
                }
                continue;
            }

            let mut parts = line.split('=');
            parts.next();
            let mut x_str = parts.next().unwrap();
            let y_str = parts.next().unwrap();
            x_str = x_str.split(',').next().unwrap();

            puzzle.answer = (x_str.parse().unwrap(), y_str.trim().parse().unwrap());
            has_answer = true;
        }

        if !has_answer {
            continue;
        }

        puzzles.push(puzzle);
    }

    let mut result_p1 = 0;
    for puzzle in puzzles.iter() {
        if let Some(price) = puzzle.solve() {
            result_p1 += price;
        }
    }
    println!("{}", result_p1);

    let mut result_p2 = 0;
    for puzzle in puzzles.iter_mut() {
        puzzle.answer.0 *= 10000000000000;
        puzzle.answer.1 *= 10000000000000;
        if let Some(price) = puzzle.solve() {
            result_p2 += price;
        }
    }

    println!("{}", result_p2);

    println!("Elapsed: {:.2?}", now.elapsed());
}

impl Puzzle {
    fn solve(&self) -> Option<usize> {
        let potential_solusions = combinations_result_in(self.a, self.b, self.answer);

        match potential_solusions.len() {
            0 => None,
            1 => Some(calculate_price(potential_solusions[0])),
            _ => {
                let mut prices: Vec<usize> = potential_solusions
                    .iter()
                    .map(|s| calculate_price(*s))
                    .collect();

                prices.sort();

                Some(prices[0])
            }
        }
    }
}

fn calculate_price((a, b): (usize, usize)) -> usize {
    (a * 3) + b
}

fn combinations_result_in(
    a: (usize, usize),
    b: (usize, usize),
    price: (usize, usize),
) -> Vec<(usize, usize)> {
    let bx_fits_total_of = price.0 / b.0;
    let by_fits_total_of = price.1 / b.1;

    let b_range_max = min(bx_fits_total_of, by_fits_total_of);
    let b_range_min = if b_range_max > 100_000 {
        b_range_max - 100_000
    } else {
        0
    };

    let mut solusions = Vec::new();
    for b_times in (b_range_min..=min(bx_fits_total_of, by_fits_total_of)).rev() {
        let x = b_times * b.0;
        let y = b_times * b.1;

        if x == price.0 && y == price.1 {
            // Exact match
            solusions.push((0, b_times));
            continue;
        }

        let x_remainder = price.0 - x;
        let y_remainder = price.1 - y;

        if x_remainder % a.0 != 0 || y_remainder % a.1 != 0 {
            continue;
        }

        let ax_times = x_remainder / a.0;
        let ay_times = y_remainder / a.1;
        if ax_times != ay_times {
            continue;
        }

        solusions.push((ax_times, b_times));
    }

    if b_range_min > 0 {
        for b_times in 0..=100_000 {
            let x = b_times * b.0;
            let y = b_times * b.1;

            if x == price.0 && y == price.1 {
                // Exact match
                solusions.push((0, b_times));
                continue;
            }

            let x_remainder = price.0 - x;
            let y_remainder = price.1 - y;

            if x_remainder % a.0 != 0 || y_remainder % a.1 != 0 {
                continue;
            }

            let ax_times = x_remainder / a.0;
            let ay_times = y_remainder / a.1;
            if ax_times != ay_times {
                continue;
            }

            solusions.push((ax_times, b_times));
        }
    }

    solusions

    // let a_cheap_total = result / a.1;
    // let b_cheap_total = result / b.1;
    // let cheap_total = max(a_cheap_total, b_cheap_total);

    // let b_fits_total_of = result / a.1;
    // let a_fits_total_of = max(result / a.0, 1000);

    // let b_fits_total_of_min = if b_fits_total_of > 1000 {
    //     b_fits_total_of - 1000
    // } else {
    //     0
    // };

    // let mut response: Vec<(usize, usize)> = Vec::new();
    // for b_times in b_fits_total_of_min..=b_fits_total_of {
    //     let b_total = b_times * a.1;
    //     if b_total == result {
    //         return vec![(0, b_times)];
    //     }

    //     let remainder = result - b_total;
    //     if remainder % a.0 != 0 {
    //         continue;
    //     }

    //     for a_times in 1..=a_fits_total_of {
    //         let total = a_times * a.0 + b_total;
    //         if total > result {
    //             break;
    //         }

    //         if total != result {
    //             continue;
    //         }

    //         let alternative_total = alternative_a * a_times + alternative_b * b_times;
    //         if alternative_total == result {
    //             response.push((a_times, b_times));
    //             break;
    //         }
    //     }
    // }
}
