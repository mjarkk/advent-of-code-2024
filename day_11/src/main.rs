use std::fs;
use std::thread;
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

    let mut result = 0;
    for (idx, nr) in numbers.iter().enumerate() {
        let start = Instant::now();
        result += total(*nr, 75, 1);
        println!(
            "{} / {} took {:.2?}",
            idx + 1,
            numbers.len(),
            start.elapsed()
        );
    }

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

fn total(input: usize, remaining_iterations: usize, threads: usize) -> usize {
    if remaining_iterations == 0 {
        return 1;
    }

    let new_remainder = remaining_iterations - 1;
    if input == 0 {
        // Replace with 1
        return total(1, new_remainder, threads);
    }

    if input >= 10 {
        let input_len = number_len(input);
        // let number_str = nr.to_string();
        if input_len % 2 == 0 {
            // Split in half
            let half = input_len / 2;
            let base = 10usize.pow(half);

            let right = input % base;
            let left = (input - right) / base;

            if threads >= 16 {
                return total(left, new_remainder, threads) + total(right, new_remainder, threads);
            }

            let new_threads = threads * 2;
            let new_threads_clone = new_threads.clone();
            let new_remainder_clone = new_remainder.clone();
            let left_result_thread =
                thread::spawn(move || total(left, new_remainder_clone, new_threads_clone));

            let right_result = total(right, new_remainder, new_threads);
            return left_result_thread.join().unwrap() + right_result;
        }
    }

    // Multiply by 2024
    return total(input * 2024, new_remainder, threads);
}
