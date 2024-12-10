use std::fs;
use std::time::Instant;

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    // For every 9 in the map we will store the amount of hits
    // The index for this for every 9 is stored in the map
    let mut solusions: Vec<u8> = Vec::with_capacity(256);

    let mut solver = Solver {
        map: Vec::with_capacity(50),
        size: (0, 0),
    };

    // Every position of each 0 in the map as we always start from 0
    let mut zeros: Vec<(usize, usize)> = Vec::with_capacity(256);

    for (y, line) in puzzle.lines().enumerate() {
        if line.is_empty() {
            continue;
        }

        let mut map_line: Vec<(u8, usize)> = Vec::with_capacity(50);
        for (x, c) in line.chars().enumerate() {
            if c < '0' || c > '9' {
                panic!("Invalid character: {}", c);
            }

            let c_nr = c as u8 - '0' as u8;
            if c_nr == 0 {
                map_line.push((c_nr, 0));
                zeros.push((x, y));
            } else if c_nr == 9 {
                map_line.push((c_nr, solusions.len()));
                solusions.push(0);
            } else {
                map_line.push((c_nr, 0));
            }
        }
        solver.map.push(map_line);
    }

    solver.size = (solver.map[0].len(), solver.map.len());

    let mut p1_answer = 0usize;
    let mut p2_answer = 0usize;

    for cord in zeros {
        solver.try_solve(cord, 1, &mut solusions);

        for idx in 0..solusions.len() {
            let hits = solusions[idx];
            if hits > 0 {
                p1_answer += 1;
                p2_answer += hits as usize;
                solusions[idx] = 0;
            }
        }
    }

    println!("p1: {}", p1_answer);
    println!("p2: {}", p2_answer);
    println!("Elapsed: {:.2?}", now.elapsed());
}

struct Solver {
    map: Vec<Vec<(u8, usize)>>,
    size: (usize, usize),
}

enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Solver {
    fn try_solve(&self, cord: (usize, usize), expected_height: u8, solusions: &mut Vec<u8>) {
        for next_direction in [
            Direction::Up,
            Direction::Down,
            Direction::Left,
            Direction::Right,
        ] {
            self.try_solve_direction(next_direction, cord, expected_height, solusions);
        }
    }
    fn try_solve_direction(
        &self,
        direction: Direction,
        cord: (usize, usize),
        expected_height: u8,
        solusions: &mut Vec<u8>,
    ) {
        let next_cord: (usize, usize) = match direction {
            Direction::Up => {
                if cord.1 == 0 {
                    return;
                }
                (cord.0, cord.1 - 1)
            }
            Direction::Down => {
                if cord.1 == self.size.1 - 1 {
                    return;
                }
                (cord.0, cord.1 + 1)
            }
            Direction::Left => {
                if cord.0 == 0 {
                    return;
                }
                (cord.0 - 1, cord.1)
            }
            Direction::Right => {
                if cord.0 == self.size.0 - 1 {
                    return;
                }
                (cord.0 + 1, cord.1)
            }
        };

        let (height, solusions_index) = self.map[next_cord.1][next_cord.0];
        if height != expected_height {
            return;
        }

        if expected_height == 9 {
            solusions[solusions_index] += 1;
        } else {
            self.try_solve(next_cord, expected_height + 1, solusions);
        }
    }
}
