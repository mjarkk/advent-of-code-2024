use std::collections::HashMap;
use std::fs;
use std::time::Instant;

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut numbers: Vec<usize> = Vec::new();
    for line in puzzle.lines() {
        for nr_str in line.split(' ') {
            numbers.push(nr_str.parse().unwrap());
        }
        break;
    }

    let result = total(numbers, 75);

    println!("{}", result);
    println!("Elapsed: {:.2?}", now.elapsed());
}

fn number_len(mut nr: usize) -> u32 {
    let mut nr_len = 1u32;
    loop {
        if nr < 10 {
            break;
        }

        nr = nr / 10;
        nr_len += 1;
    }

    nr_len
}

fn total(input: Vec<usize>, iterations: usize) -> usize {
    let mut set_a: HashMap<usize, usize> = HashMap::with_capacity(4000);
    let mut set_b: HashMap<usize, usize> = HashMap::with_capacity(4000);

    for nr in input {
        let prev = *set_a.get(&nr).unwrap_or(&0);
        set_a.insert(nr, prev + 1);
    }

    for i in 0..iterations {
        if i % 2 == 0 {
            set_b.clear();
            calculate_next(&mut set_a, &mut set_b);
        } else {
            set_a.clear();
            calculate_next(&mut set_b, &mut set_a);
        }
    }

    if iterations % 2 == 0 {
        set_total(set_a)
    } else {
        set_total(set_b)
    }
}

fn set_total(set: HashMap<usize, usize>) -> usize {
    let mut result = 0;
    for (_, total) in set.iter() {
        result += total;
    }
    result
}

fn calculate_next(numbers: &mut HashMap<usize, usize>, new_numbers: &mut HashMap<usize, usize>) {
    for (input, total) in numbers.iter() {
        let input = *input;
        if input == 0 {
            // Replace with 1
            let prev = *new_numbers.get(&1).unwrap_or(&0);
            new_numbers.insert(1, prev + total);

            continue;
        }

        if input >= 10 {
            let input_len = number_len(input);
            if input_len % 2 == 0 {
                // Split in half
                let half = input_len / 2;
                let base = 10usize.pow(half);

                let left = input % base;
                let right = (input - left) / base;

                let prev_left = *new_numbers.get(&left).unwrap_or(&0);
                new_numbers.insert(left, prev_left + total);

                let prev_right = *new_numbers.get(&right).unwrap_or(&0);
                new_numbers.insert(right, prev_right + total);
                continue;
            }
        }

        // Multiply by 2024
        let key = input * 2024;
        let prev = *new_numbers.get(&key).unwrap_or(&0);
        new_numbers.insert(key, prev + total);
    }
}
