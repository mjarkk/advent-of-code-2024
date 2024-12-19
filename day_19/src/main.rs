use std::fs;
use std::time::Instant;

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();
    let (towels_puzzle, combinations_puzzle) = puzzle.split_once("\n\n").unwrap();

    let mut combinations: Vec<String> = Vec::new();
    let mut towels: Vec<String> = Vec::new();
    for combination in combinations_puzzle.lines() {
        if !combination.is_empty() {
            combinations.push(combination.to_string());
        }
    }
    for towel_line in towels_puzzle.lines() {
        if towel_line.is_empty() {
            continue;
        }

        for towel in towel_line.split(", ") {
            let towel = towel.trim();
            towels.push(towel.to_string());
        }
    }

    let mut result = 0;

    let mut towels_in_combination: Vec<String> = Vec::new();
    for (idx, combination) in combinations.iter().enumerate() {
        // Find towels that appear in the combination
        towels_in_combination.clear();
        for entry in towels.iter() {
            if combination.contains(entry) {
                towels_in_combination.push(entry.to_string());
            }
        }

        // Try to solve the combination
        if try_solve(&towels_in_combination, combination, 0) {
            result += 1;
        }

        println!("solved {} of {}", idx + 1, combinations.len());
    }

    println!("{}", result);
    println!("Elapsed: {:.2?}", now.elapsed());
}

fn try_solve(towels: &Vec<String>, combination: &str, offset: usize) -> bool {
    let needle = &combination[offset..];
    for towel in towels.iter() {
        if !needle.starts_with(towel) {
            continue;
        }

        println!("towel: {}", towel);

        let new_offset = offset + towel.len();
        if new_offset == combination.len() {
            return true;
        }

        if try_solve(towels, combination, new_offset) {
            return true;
        }
    }

    false
}
