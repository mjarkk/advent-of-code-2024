use std::collections::HashSet;
use std::fs;
use std::time::Instant;

#[derive(Debug, Clone)]
enum Tail {
    Visitable(Option<usize>),
    Wall,
}

struct State {
    map: Vec<Vec<Tail>>,
    end: (usize, usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Directions {
    Up,
    Down,
    Left,
    Right,
}

impl Directions {
    fn next_direction(&self) -> [Self; 3] {
        match self {
            Directions::Up => [Directions::Up, Directions::Left, Directions::Right],
            Directions::Down => [Directions::Down, Directions::Right, Directions::Left],
            Directions::Left => [Directions::Left, Directions::Down, Directions::Up],
            Directions::Right => [Directions::Right, Directions::Up, Directions::Down],
        }
    }
    fn get_cords(&self, cords: (usize, usize)) -> (usize, usize) {
        match self {
            Directions::Up => (cords.0, cords.1 - 1),
            Directions::Down => (cords.0, cords.1 + 1),
            Directions::Left => (cords.0 - 1, cords.1),
            Directions::Right => (cords.0 + 1, cords.1),
        }
    }
}

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut state = State {
        map: Vec::new(),
        end: (0, 0),
    };
    let mut start = (0, 0);

    for line in puzzle.lines() {
        if line.is_empty() {
            continue;
        }

        let mut row = Vec::new();
        for c in line.chars() {
            row.push(match c {
                '.' => Tail::Visitable(None),
                '#' => Tail::Wall,
                'E' => {
                    state.end = (row.len(), state.map.len());
                    Tail::Visitable(None)
                }
                'S' => {
                    start = (row.len(), state.map.len());
                    Tail::Visitable(None)
                }
                c => panic!("Unknown character: {}", c),
            })
        }

        state.map.push(row);
    }

    let (answer_p1, visited) = state
        .find_lowest_cost_path_to_end(start, &Directions::Right, 0)
        .unwrap();
    println!("p1: {}", answer_p1);

    let mut visited_set = HashSet::new();
    for cords in visited.iter() {
        visited_set.insert(*cords);
    }

    println!("p2: {}", visited_set.len());

    // for (y, line) in state.map.iter().enumerate() {
    //     for (x, tail) in line.iter().enumerate() {
    //         if visited_set.contains(&(x, y)) {
    //             print!("+");
    //             continue;
    //         }
    //         match tail {
    //             Tail::Visitable(Some(_)) => print!("."),
    //             Tail::Visitable(None) => print!("."),
    //             Tail::Wall => print!("#"),
    //         }
    //     }
    //     println!();
    // }

    println!("Elapsed: {:.2?}", now.elapsed());
}

impl State {
    // Finds the path with the lowest cost to the end of the map
    fn find_lowest_cost_path_to_end(
        &mut self,
        cords: (usize, usize),
        direction: &Directions,
        cost: usize,
    ) -> Option<(usize, Vec<(usize, usize)>)> {
        if cords == self.end {
            return Some((cost, vec![cords]));
        }

        let optional_tail_cost = match &self.map[cords.1][cords.0] {
            Tail::Wall => return None,
            Tail::Visitable(v) => v,
        };
        if let Some(tail_cost) = optional_tail_cost {
            if cost > *tail_cost && (cost < 1000 || cost - 1000 != *tail_cost) {
                return None;
            }
        }

        self.map[cords.1][cords.0] = Tail::Visitable(Some(cost));

        let mut fastest = None;

        for (idx, new_direction) in direction.next_direction().iter().enumerate() {
            let mut new_cost = cost + 1;
            if idx != 0 {
                new_cost += 1000;
            }

            let new_cords = new_direction.get_cords(cords);
            let (cost, mut path) =
                match self.find_lowest_cost_path_to_end(new_cords, new_direction, new_cost) {
                    None => continue,
                    Some(v) => v,
                };

            let (fastest_cost, fastest_path) = match &fastest {
                None => {
                    fastest = Some((cost, path));
                    continue;
                }
                Some(v) => v,
            };

            if cost > *fastest_cost {
                // This path cost more than the fastest path found so far
                continue;
            }

            if cost < *fastest_cost {
                // This path is faster than the fastest path found so far
                fastest = Some((cost, path));
                continue;
            }

            // This path is as fast as the fastest path found so far
            // Add the current fastest path parts to the current path
            for entry in fastest_path.iter() {
                path.push(*entry);
            }

            fastest = Some((cost, path));
        }

        if let Some((cost, mut path)) = fastest {
            path.push(cords);
            fastest = Some((cost, path));
        }

        fastest
    }
}
