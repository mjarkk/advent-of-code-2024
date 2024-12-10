use std::collections::{HashMap, HashSet};
use std::fs;
use std::time::Instant;

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut solver = Solver {
        map: HashMap::new(),
    };
    for (y, line) in puzzle.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c < '0' || c > '9' {
                panic!("Invalid character: {}", c);
            }
            let c_nr = c as u8 - '0' as u8;
            solver.map.insert((x, y), c_nr);
        }
    }

    let mut p1_answer = 0;
    let mut p2_answer = 0;

    for (cord, nr) in solver.map.iter() {
        if *nr != 0 {
            continue;
        }

        let mut all_paths: Vec<(usize, usize)> = Vec::new();
        solver.try_solve(cord, 1, &mut all_paths);
        p2_answer += all_paths.len();

        let mut paths_with_unique_end: HashSet<(usize, usize)> = HashSet::new();
        for path in all_paths {
            paths_with_unique_end.insert(path);
        }
        p1_answer += paths_with_unique_end.len();
    }

    println!("p1: {}", p1_answer);
    println!("p2: {}", p2_answer);
    println!("Elapsed: {:.2?}", now.elapsed());
}

struct Solver {
    map: HashMap<(usize, usize), u8>,
}

impl Solver {
    fn try_solve(
        &self,
        cord: &(usize, usize),
        expected_next_nr: u8,
        solusions: &mut Vec<(usize, usize)>,
    ) {
        let next_cords_offsets: [(isize, isize); 4] = [(1, 0), (-1, 0), (0, 1), (0, -1)];

        for next_cord_offset in next_cords_offsets.iter() {
            let next_cord = (
                (cord.0 as isize + next_cord_offset.0),
                (cord.1 as isize + next_cord_offset.1),
            );
            if next_cord.0 < 0 || next_cord.1 < 0 {
                continue;
            }
            let next_cord = (next_cord.0 as usize, next_cord.1 as usize);

            match self.map.get(&next_cord) {
                Some(nr) if *nr == expected_next_nr => nr,
                _ => continue,
            };

            if expected_next_nr == 9 {
                solusions.push(next_cord);
            } else {
                self.try_solve(&next_cord, expected_next_nr + 1, solusions);
            }
        }
    }
}
