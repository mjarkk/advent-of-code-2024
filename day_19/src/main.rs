use std::fs;
use std::time::Instant;

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();
    let (towels_puzzle, combinations_puzzle) = puzzle.split_once("\n\n").unwrap();

    let mut combinations: Vec<String> = Vec::new();
    let mut longest_combination = 0;
    let mut towels: Vec<String> = Vec::new();
    let mut longest_towel = 0;
    for combination in combinations_puzzle.lines() {
        if !combination.is_empty() {
            combinations.push(combination.to_string());

            if combination.len() > longest_combination {
                longest_combination = combination.len();
            }
        }
    }
    for towel_line in towels_puzzle.lines() {
        if towel_line.is_empty() {
            continue;
        }

        for towel in towel_line.split(", ") {
            let towel = towel.trim();
            towels.push(towel.to_string());

            if towel.len() > longest_towel {
                longest_towel = towel.len();
            }
        }
    }

    let mut result_p1 = 0;
    let mut result_p2 = 0;

    let mut checked_lens: Vec<Option<usize>> = Vec::new();
    for _ in 0..longest_combination + 1 {
        checked_lens.push(None);
    }

    for combination in combinations.iter() {
        // Clear the checked lens list
        for idx in 0..checked_lens.len() {
            checked_lens[idx] = None;
        }

        // Try to solve the combination
        let total = try_solve(&towels, combination, &mut checked_lens, 0);
        if total > 0 {
            result_p1 += 1;
            result_p2 += total;
        }
    }

    println!("{}", result_p1);
    println!("{}", result_p2);
    println!("Elapsed: {:.2?}", now.elapsed());
}

fn try_solve(
    towels: &Vec<String>,
    combination: &str,
    checked_lens: &mut Vec<Option<usize>>,
    offset: usize,
) -> usize {
    let mut result = 0;
    let needle = &combination[offset..];
    for towel in towels.iter() {
        let new_offset = offset + towel.len();
        if new_offset > combination.len() {
            continue;
        }

        if !needle.starts_with(towel) {
            continue;
        }

        if let Some(v) = checked_lens[new_offset] {
            result += v;
            continue;
        }

        if new_offset == combination.len() {
            result += 1;
            continue;
        }

        result += try_solve(towels, combination, checked_lens, new_offset);
    }

    checked_lens[offset] = Some(result);

    result
}
