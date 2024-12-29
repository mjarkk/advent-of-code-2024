use std::collections::HashMap;
use std::fs;
use std::time::Instant;

const CAP: usize = 4_000;

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

    let mut state = State {
        lookups: HashMap::with_capacity(CAP),
        reverse_lookups: HashMap::with_capacity(CAP),
        lookup_offset: 0,
        step_cache: Vec::with_capacity(CAP),
    };
    let result = state.total(numbers, 75);

    println!("{}", result);
    println!("Elapsed: {:.2?}", now.elapsed());
}

struct State {
    lookups: HashMap<usize, usize>,
    reverse_lookups: HashMap<usize, usize>,
    lookup_offset: usize,
    step_cache: Vec<StepAction>,
}

enum StepAction {
    Unknown,
    Split(usize, usize),
    Replace(usize),
}

impl State {
    fn new_lookup(&mut self, source: usize) -> usize {
        if let Some(idx) = self.lookups.get(&source) {
            return *idx;
        }

        let idx = self.lookup_offset;
        self.lookups.insert(source, idx);
        self.reverse_lookups.insert(idx, source);
        self.lookup_offset += 1;

        idx
    }
    fn total(&mut self, input: Vec<usize>, iterations: usize) -> usize {
        let mut set_a: Vec<usize> = Vec::with_capacity(CAP);
        let mut set_b: Vec<usize> = Vec::with_capacity(CAP);

        for _ in 0..CAP {
            set_a.push(0);
            set_b.push(0);
            self.step_cache.push(StepAction::Unknown);
        }

        for nr in input {
            let idx = self.new_lookup(nr);
            set_a[idx] += 1;
        }

        for i in 0..iterations {
            if i % 2 == 0 {
                for idx in 0..set_b.len() {
                    set_b[idx] = 0;
                }
                self.calculate_next(&mut set_a, &mut set_b);
            } else {
                for idx in 0..set_a.len() {
                    set_a[idx] = 0;
                }
                self.calculate_next(&mut set_b, &mut set_a);
            }
        }

        if iterations % 2 == 0 {
            self.set_total(set_a)
        } else {
            self.set_total(set_b)
        }
    }

    fn set_total(&mut self, set: Vec<usize>) -> usize {
        let mut result = 0;
        for total in set.iter() {
            result += total;
        }

        result
    }
    fn calculate_next(&mut self, numbers: &mut Vec<usize>, new_numbers: &mut Vec<usize>) {
        for (input_index, total) in numbers.iter().enumerate() {
            if *total == 0 {
                continue;
            }

            match self.step_cache[input_index] {
                StepAction::Unknown => { /* Do nothing */ }
                StepAction::Split(left, right) => {
                    new_numbers[left] += total;
                    new_numbers[right] += total;
                    continue;
                }
                StepAction::Replace(replace) => {
                    new_numbers[replace] += total;
                    continue;
                }
            }

            let input = *self.reverse_lookups.get(&input_index).unwrap();
            if input == 0 {
                // Replace all zeros with ones
                let key = self.new_lookup(1);
                new_numbers[key] += total;
                self.step_cache[input_index] = StepAction::Replace(key);
                continue;
            }

            let input_len = number_len(input);
            if input_len % 2 == 0 {
                // Split al evenly length numbers in half
                let half = input_len / 2;
                let base = 10usize.pow(half);

                let left = input % base;
                let right = (input - left) / base;

                let left_key = self.new_lookup(left);
                let right_key = self.new_lookup(right);

                new_numbers[left_key] += total;
                new_numbers[right_key] += total;
                self.step_cache[input_index] = StepAction::Split(left_key, right_key);
                continue;
            }

            // Multiply remainder by 2024
            let key = self.new_lookup(input * 2024);
            new_numbers[key] += total;
            self.step_cache[input_index] = StepAction::Replace(key);
        }
    }
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
