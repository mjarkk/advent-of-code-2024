use std::fs;
use std::time::Instant;

// Test case:
// const MAP_SIZE: usize = 6;
// const CUT_OFF: usize = 12;

// Puzzle:
const MAP_SIZE: usize = 70;
const CUT_OFF: usize = 1024;

#[derive(Debug, Clone)]
enum Tail {
    Visitable(Option<usize>),
    Corrupt,
}

struct State {
    map: Vec<Vec<Tail>>,
}

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut state = State { map: Vec::new() };
    let mut row = Vec::new();
    for _ in 0..MAP_SIZE + 1 {
        row.push(Tail::Visitable(None));
    }
    for _ in 0..MAP_SIZE + 1 {
        state.map.push(row.clone());
    }

    let mut corrupted_areas = Vec::new();
    for (idx, line) in puzzle.lines().enumerate() {
        if line.is_empty() {
            continue;
        }

        let (x_str, y_str) = line.split_once(',').unwrap();
        let x = x_str.parse::<usize>().unwrap();
        let y = y_str.parse::<usize>().unwrap();

        corrupted_areas.push((x, y));

        if idx < CUT_OFF {
            state.map[y][x] = Tail::Corrupt;
        }
    }

    let answer_p1 = state.find_lowest_cost_path_to_end(0, 0, 0).unwrap();
    println!("p1: {}", answer_p1);

    let mut attempts = 0;
    let mut path: Vec<(usize, usize)> = Vec::new();
    for idx in CUT_OFF..corrupted_areas.len() {
        attempts += 1;

        // Add the new corrupted area
        let (x, y) = corrupted_areas[idx];
        state.map[y][x] = Tail::Corrupt;

        if !path.contains(&(x, y)) && idx != CUT_OFF {
            continue;
        }
        path.clear();

        // Reset the map state
        for y in 0..=MAP_SIZE {
            for x in 0..=MAP_SIZE {
                if let Tail::Visitable(Some(_)) = state.map[y][x] {
                    state.map[y][x] = Tail::Visitable(None);
                }
            }
        }

        if !state.can_reach_end(0, 0, &mut path) {
            println!("p2: {},{}", x, y);
            println!("Attempts: {}", attempts);
            break;
        }
    }

    println!("Elapsed: {:.2?}", now.elapsed());
}

impl State {
    // Check if we can find a path to the end of the map
    // Compared to find_lowest_cost_path_to_end, this function is faster as we can stop as soon as we find a path
    fn can_reach_end(&mut self, x: usize, y: usize, path: &mut Vec<(usize, usize)>) -> bool {
        if x == MAP_SIZE && y == MAP_SIZE {
            path.push((x, y));
            return true;
        }

        let tail = &self.map[y][x];
        match tail {
            Tail::Corrupt => return false,
            Tail::Visitable(Some(_)) => return false,
            Tail::Visitable(None) => { /* Continue */ }
        }

        self.map[y][x] = Tail::Visitable(Some(1));

        // Check down:
        if y < MAP_SIZE && self.can_reach_end(x, y + 1, path) {
            path.push((x, y));
            return true;
        }

        // Check right:
        if x < MAP_SIZE && self.can_reach_end(x + 1, y, path) {
            path.push((x, y));
            return true;
        }

        // Check up:
        if y > 0 && self.can_reach_end(x, y - 1, path) {
            path.push((x, y));
            return true;
        }

        // Check left:
        if x > 0 && self.can_reach_end(x - 1, y, path) {
            path.push((x, y));
            return true;
        }

        false
    }

    // Finds the path with the lowest cost to the end of the map
    fn find_lowest_cost_path_to_end(&mut self, x: usize, y: usize, cost: usize) -> Option<usize> {
        if x == MAP_SIZE && y == MAP_SIZE {
            return Some(cost);
        }

        let tail = &self.map[y][x];
        match tail {
            Tail::Corrupt => return None,
            Tail::Visitable(Some(tail_cost)) if *tail_cost <= cost => return None,
            Tail::Visitable(_) => { /* Continue */ }
        };

        self.map[y][x] = Tail::Visitable(Some(cost));
        let new_cost = cost + 1;

        let mut fastest = None;

        // Check down:
        if y < MAP_SIZE {
            match (
                self.find_lowest_cost_path_to_end(x, y + 1, new_cost),
                fastest,
            ) {
                (Some(v), None) => fastest = Some(v),
                (Some(result), Some(fastest_value)) if result < fastest_value => {
                    fastest = Some(result)
                }
                _ => { /* continue */ }
            }
        }

        // Check right:
        if x < MAP_SIZE {
            match (
                self.find_lowest_cost_path_to_end(x + 1, y, new_cost),
                fastest,
            ) {
                (Some(v), None) => fastest = Some(v),
                (Some(result), Some(fastest_value)) if result < fastest_value => {
                    fastest = Some(result)
                }
                _ => { /* continue */ }
            }
        }

        // Check up:
        if y > 0 {
            match (
                self.find_lowest_cost_path_to_end(x, y - 1, new_cost),
                fastest,
            ) {
                (Some(v), None) => fastest = Some(v),
                (Some(result), Some(fastest_value)) if result < fastest_value => {
                    fastest = Some(result)
                }
                _ => { /* continue */ }
            }
        }

        // Check left:
        if x > 0 {
            match (
                self.find_lowest_cost_path_to_end(x - 1, y, new_cost),
                fastest,
            ) {
                (Some(v), None) => fastest = Some(v),
                (Some(result), Some(fastest_value)) if result < fastest_value => {
                    fastest = Some(result)
                }
                _ => { /* continue */ }
            }
        }

        fastest
    }

    fn _debug_print(&self) {
        for y in 0..=MAP_SIZE {
            for x in 0..=MAP_SIZE {
                if x > 0 {
                    print!("|");
                }
                match self.map[y][x] {
                    Tail::Corrupt => print!("##"),
                    Tail::Visitable(Some(cost)) => {
                        let cost_str = cost.to_string();
                        match cost_str.len() {
                            1 => print!("{} ", cost_str),
                            2 => print!("{}", cost_str),
                            l => print!("{}", cost_str[l - 2..].to_string()),
                        }
                    }
                    Tail::Visitable(None) => print!("  "),
                }
            }
            println!();
        }
        println!();
    }
}
