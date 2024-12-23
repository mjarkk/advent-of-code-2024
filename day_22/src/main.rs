use std::fs;
use std::time::Instant;

const PRUNE: usize = 16777216;

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut result = 0;
    for line in puzzle.lines() {
        if line.is_empty() {
            continue;
        }

        let mut secret: usize = line.parse().unwrap();
        for _ in 0..2000 {
            gfcret(secret);
        }
        result += secret;
    }

    println!("{}", result);
    println!("Elapsed: {:.2?}", now.elapsed());
}

fn next_secret(mut secret: usize) -> usize {
    // Calculate the result of multiplying the secret number by 64.
    // Then, mix this result into the secret number. Finally, prune the secret number.
    secret ^= secret * 64;
    secret %= PRUNE;

    // Calculate the result of dividing the secret number by 32.
    // Round the result down to the nearest integer.
    // Then, mix this result into the secret number.
    // Finally, prune the secret number.
    secret ^= secret / 32;
    secret %= PRUNE;

    // Calculate the result of multiplying the secret number by 2048.
    // Then, mix this result into the secret number.
    // Finally, prune the secret number.
    secret ^= secret * 2048;
    secret %= PRUNE;

    return secret;
}
