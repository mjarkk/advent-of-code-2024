mod pads;

use std::fs;
use std::time::Instant;

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut result_p1 = 0;
    let mut result_p2 = 0;
    for line in puzzle.lines() {
        if line.is_empty() {
            continue;
        }

        let nr: usize = line[..line.len() - 1].parse().unwrap();

        let mut source_moves: Vec<char> = line.chars().collect();
        source_moves = pads::Pad::Numpad.calculate_moves(&source_moves);

        let mut pad_solver = pads::PadSolver::new();
        let line_p1 = pad_solver.solve_recursive(&source_moves, 3);
        result_p1 += line_p1 * nr;

        let line_p2 = pad_solver.solve_recursive(&source_moves, 26);
        result_p2 += line_p2 * nr;
    }

    println!("{}", result_p1);
    println!("{}", result_p2);
    println!("Elapsed: {:.2?}", now.elapsed());
}
