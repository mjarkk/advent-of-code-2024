use std::collections::HashMap;
use std::fs;
use std::time::Instant;

const PRUNE: usize = 16777216;

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut lookup_key_mask = 0u32;
    for idx in 0u32..(6 * 4) {
        lookup_key_mask = lookup_key_mask | 1 << idx;
    }

    let mut global_price_trakcer: HashMap<u32, u16> = HashMap::new();

    let mut result = 0;
    for line in puzzle.lines() {
        if line.is_empty() {
            continue;
        }

        let mut monkey_price_trakcer: HashMap<u32, u16> = HashMap::new();

        let mut secret: usize = line.parse().unwrap();
        let mut prev_price = (secret % 10) as i8;
        let mut lookup_key = 0u32;
        for idx in 0..2000 {
            secret = next_secret(secret);
            let price = (secret % 10) as i8;
            let price_diff = price - prev_price;
            // println!("{}: {} ({})", secret, price, price_diff);

            lookup_key = (lookup_key << 6) | (price_diff + 10) as u32;
            if idx > 2 {
                let key = lookup_key & lookup_key_mask;
                if !monkey_price_trakcer.contains_key(&key) {
                    monkey_price_trakcer.insert(key, price as u16);
                }
            }

            prev_price = price;
        }
        result += secret;

        for (key, value) in monkey_price_trakcer {
            let entry = global_price_trakcer.entry(key).or_insert(0);
            *entry += value;
        }
    }
    println!("p1: {}", result);

    let mut result = 0u16;
    for value in global_price_trakcer.values() {
        if *value > result {
            result = *value;
        }
    }
    println!("p2: {}", result);

    println!("Elapsed: {:.2?}", now.elapsed());
}

// fn set_nr_to_set(nr: u32) -> [i8; 4] {
//     let mut resp = [0, 0, 0, 0];
//     let mask = 0x3F;
//     for i in 0..4 {
//         resp[3 - i] = (nr >> (i * 6) & mask) as i8 - 10;
//     }
//     resp
// }

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
