use std::fs;
use std::time::Instant;

const PRUNE: usize = 16777216;
const LOOKUP_KEY_FLAGS_MASK: u32 = (1 << 4) - 1;

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    // This doesn't feel great but is just 20x faster than a hashmap.
    let mut global_price_tracker = [0u16; 262_143];
    let mut monkey_price_tracker = [0u16; 262_143];

    let mut result = 0;
    for line in puzzle.lines() {
        if line.is_empty() {
            continue;
        }

        let mut secret: usize = line.parse().unwrap();
        let mut prev_price = (secret % 10) as i8;
        let mut lookup_key_negative_flags = 0u32;
        let mut lookup_key = 0u32;
        for idx in 0..2000 {
            secret = next_secret(secret);
            let price = (secret % 10) as i8;
            let price_diff = price - prev_price;

            // Track the changes to the price in 1 number.
            // The first 4 digits are the absolute changes to the price.
            // After the digets of a number that contains binary flags for each price change if it's a negative number.
            lookup_key = ((lookup_key * 10) + (price_diff.abs() as u32)) % 10000;
            lookup_key_negative_flags =
                (lookup_key_negative_flags << 1) | if price_diff < 0 { 1 } else { 0 };
            if idx > 2 {
                let key = ((lookup_key_negative_flags & LOOKUP_KEY_FLAGS_MASK) * 10000 + lookup_key)
                    as usize;

                if monkey_price_tracker[key] == 0 {
                    monkey_price_tracker[key] = price as u16;
                }
            }

            prev_price = price;
        }
        result += secret;

        for (idx, value) in monkey_price_tracker.iter_mut().enumerate() {
            global_price_tracker[idx] += *value;
            *value = 0;
        }
    }
    println!("p1: {}", result);

    let mut result = 0u16;
    for value in global_price_tracker {
        if value > result {
            result = value;
        }
    }
    println!("p2: {}", result);

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
