use std::fs;
use std::time::Instant;

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let result = 0;

    // Code here

    println!("{}", result);
    println!("Elapsed: {:.2?}", now.elapsed());
}
