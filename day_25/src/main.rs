use std::fs;
use std::time::Instant;

const SIZE: usize = 5;

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut locks = Vec::new();
    let mut keys = Vec::new();

    for key_or_lock_lines in puzzle.split("\n\n") {
        let lines: Vec<Vec<char>> = key_or_lock_lines
            .lines()
            .map(|l| l.chars().collect())
            .collect();

        if !lines.first().unwrap().contains(&'.') {
            // is lock
            let mut lock = Vec::new();
            for column_idx in 0..lines[0].len() {
                let mut height = 0u8;
                for line_idx in 1..lines.len() {
                    if lines[line_idx][column_idx] == '.' {
                        break;
                    }
                    height += 1;
                }
                lock.push(height);
            }
            assert_eq!(lock.len(), SIZE);
            locks.push(lock);
            continue;
        }
        if !lines.last().unwrap().contains(&'.') {
            // is key
            let mut key = Vec::new();
            for column_idx in 0..lines[0].len() {
                let mut height = 0u8;
                for line_idx in (0..lines.len() - 1).rev() {
                    if lines[line_idx][column_idx] == '.' {
                        break;
                    }
                    height += 1;
                }
                key.push(height);
            }
            assert_eq!(key.len(), SIZE);
            keys.push(key);
            continue;
        }
        panic!("neither key nor lock, very sus");
    }

    let mut result = 0;
    for lock in locks.iter() {
        'outer: for key in keys.iter() {
            for idx in 0..SIZE {
                if lock[idx] + key[idx] > SIZE as u8 {
                    continue 'outer;
                }
            }
            result += 1;
        }
    }
    println!("{}", result);

    println!("Elapsed: {:.2?}", now.elapsed());
}
