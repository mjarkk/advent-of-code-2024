use std::fs;
use std::time::Instant;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Tail {
    Empty(Option<usize>),
    Wall,
}

impl Tail {
    fn is_empty(&self) -> bool {
        match self {
            Tail::Empty(_) => true,
            _ => false,
        }
    }
}

struct State {
    map: Vec<Vec<Tail>>,
    end: (usize, usize),
    map_size: (usize, usize),
}

enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn resolve(&self, cord: (usize, usize)) -> (usize, usize) {
        match self {
            Direction::Up => (cord.0, cord.1 - 1),
            Direction::Down => (cord.0, cord.1 + 1),
            Direction::Left => (cord.0 - 1, cord.1),
            Direction::Right => (cord.0 + 1, cord.1),
        }
    }
}

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut state = State {
        map: Vec::new(),
        end: (0, 0),
        map_size: (0, 0),
    };

    let mut start = (0, 0);
    for (y, puzzle_line) in puzzle.lines().enumerate() {
        let mut map_line: Vec<Tail> = Vec::new();
        for (x, c) in puzzle_line.chars().enumerate() {
            map_line.push(match c {
                'S' => {
                    start = (x, y);
                    Tail::Empty(None)
                }
                'E' => {
                    state.end = (x, y);
                    Tail::Empty(None)
                }
                '#' => Tail::Wall,
                _ => Tail::Empty(None),
            });
        }
        state.map.push(map_line);
    }
    state.map_size = (state.map[0].len(), state.map.len());

    // Solve the puzzle first once without cheats to get to know the fastest route
    let route_cost = state.solve(start, 0).unwrap();
    println!("{}", route_cost);

    let mut cheats = 0;
    let cheat_options = state.find_places_to_cheat();
    for cheat_position in cheat_options.iter() {
        state.reset();

        // Remove the wall from the map where we can cheat
        state.map[cheat_position.1][cheat_position.0] = Tail::Empty(None);

        // Calculate a new route score
        let route_cost_with_cheat = state.solve(start, 0).unwrap();

        // Restore the cheat back to a wall
        state.map[cheat_position.1][cheat_position.0] = Tail::Wall;

        if route_cost_with_cheat >= route_cost {
            continue;
        }

        let diff = route_cost - route_cost_with_cheat;
        if diff < 100 {
            continue;
        }

        cheats += 1;
    }
    println!("{}", cheats);

    // for line in state.map.iter() {
    //     for tail in line.iter() {
    //         match tail {
    //             Tail::Empty(Some(v)) => print!("{:02} ", v),
    //             Tail::Empty(None) => print!("?? "),
    //             Tail::Wall => print!("###"),
    //         }
    //     }
    //     println!();
    // }

    println!("Elapsed: {:.2?}", now.elapsed());
}

impl State {
    fn find_places_to_cheat(&self) -> Vec<(usize, usize)> {
        let mut resp = Vec::new();

        for y in 1..self.map_size.1 - 1 {
            for x in 1..self.map_size.0 - 1 {
                if let Tail::Empty(_) = self.map[y][x] {
                    continue;
                }

                let left_right = [&self.map[y][x - 1], &self.map[y][x + 1]]
                    .iter()
                    .all(|v| v.is_empty());
                if left_right {
                    resp.push((x, y));
                    continue;
                }

                let top_bottom = [&self.map[y - 1][x], &self.map[y + 1][x]]
                    .iter()
                    .all(|v| v.is_empty());
                if top_bottom {
                    resp.push((x, y));
                    continue;
                }
            }
        }

        resp
    }

    fn solve(&mut self, cord: (usize, usize), cost: usize) -> Option<usize> {
        if cord == self.end {
            return Some(cost);
        }

        match &self.map[cord.1][cord.0] {
            Tail::Empty(Some(v)) if cost >= *v => {
                return None;
            }
            Tail::Empty(_) => { /* continue */ }
            Tail::Wall => return None,
        }

        self.map[cord.1][cord.0] = Tail::Empty(Some(cost));

        let mut best_score = None;
        for direction in [
            Direction::Up,
            Direction::Down,
            Direction::Left,
            Direction::Right,
        ] {
            let new_cord = direction.resolve(cord);
            match (self.solve(new_cord, cost + 1), best_score) {
                (None, _) => continue,
                (Some(v), None) => best_score = Some(v),
                (Some(a), Some(b)) if a < b => best_score = Some(a),
                _ => continue,
            }
        }

        best_score
    }

    fn reset(&mut self) {
        for line in self.map.iter_mut() {
            for tail in line.iter_mut() {
                if let Tail::Empty(v) = tail {
                    *v = None;
                }
            }
        }
    }
}
