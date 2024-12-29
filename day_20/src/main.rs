use std::fs;
use std::time::Instant;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum TailKind {
    Empty,
    Wall,
}

#[derive(Debug, Clone)]
struct Tail {
    kind: TailKind,
    cost: Option<usize>,
}

impl Tail {
    fn new(kind: TailKind) -> Self {
        Tail { kind, cost: None }
    }
}

struct State {
    map: Vec<Vec<Tail>>,
    solusion_cost_map: Vec<Vec<Tail>>,
    end: (usize, usize),
    start: (usize, usize),
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
        solusion_cost_map: Vec::new(),
        end: (0, 0),
        start: (0, 0),
        map_size: (0, 0),
    };

    for (y, puzzle_line) in puzzle.lines().enumerate() {
        let mut map_line: Vec<Tail> = Vec::new();
        for (x, c) in puzzle_line.chars().enumerate() {
            map_line.push(match c {
                'S' => {
                    state.start = (x, y);
                    Tail::new(TailKind::Empty)
                }
                'E' => {
                    state.end = (x, y);
                    Tail::new(TailKind::Empty)
                }
                '#' => Tail::new(TailKind::Wall),

                _ => Tail::new(TailKind::Empty),
            });
        }
        state.map.push(map_line);
    }
    state.map_size = (state.map[0].len(), state.map.len());

    state.find_cheats();

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
    fn solve(&mut self, cord: (usize, usize), cost: usize) -> Option<usize> {
        let tail = &self.map[cord.1][cord.0];
        if TailKind::Wall == tail.kind {
            return None;
        }

        if let Some(v) = tail.cost {
            if cost >= v {
                return None;
            }
        }

        self.map[cord.1][cord.0].cost = Some(cost);

        if cord == self.end {
            return Some(cost);
        }

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
                tail.cost = None;
            }
        }
    }

    fn solve_reverse(&mut self, cord: (usize, usize), cost: usize) -> Option<usize> {
        let tail = &self.map[cord.1][cord.0];
        if TailKind::Wall == tail.kind {
            return None;
        }

        if let Some(v) = tail.cost {
            if cost >= v {
                return None;
            }
        }

        self.map[cord.1][cord.0].cost = Some(cost);

        if cord == self.start {
            return Some(cost);
        }

        let mut best_score = None;
        for direction in [
            Direction::Up,
            Direction::Down,
            Direction::Left,
            Direction::Right,
        ] {
            let new_cord = direction.resolve(cord);
            match (self.solve_reverse(new_cord, cost + 1), best_score) {
                (None, _) => continue,
                (Some(v), None) => best_score = Some(v),
                (Some(a), Some(b)) if a < b => best_score = Some(a),
                _ => continue,
            }
        }

        best_score
    }

    fn find_cheats(&mut self) {
        self.solve_reverse(self.end, 0);
        self.solusion_cost_map = self.map.clone();

        self.reset();
        let min_score = if self.map_size.0 == 15 { 50 } else { 100 };
        let minimal_cost = self.solve(self.start, 0).unwrap() - min_score;

        let mut total_cheats = 0;
        let mut total_cheats_p1 = 0;
        let mut total_cheats_p2 = 0;
        for y in 1..self.map_size.1 - 1 {
            for x in 1..self.map_size.0 - 1 {
                for cheat_x_offset in -20i16..=20i16 {
                    for cheat_y_offset in -20i16..=20i16 {
                        let diff = cheat_x_offset.abs() + cheat_y_offset.abs();
                        if diff > 20 {
                            continue;
                        }
                        if diff < 2 {
                            continue;
                        }
                        if diff == 2 && cheat_x_offset.abs() == 1 {
                            continue;
                        }

                        let cheat_x = x as i16 + cheat_x_offset;
                        let cheat_y = y as i16 + cheat_y_offset;
                        if cheat_x <= 0 || cheat_x >= self.map_size.0 as i16 - 1 {
                            continue;
                        }
                        if cheat_y <= 0 || cheat_y >= self.map_size.1 as i16 - 1 {
                            continue;
                        }

                        let map_start = &self.map[y][x];
                        let map_end = &self.map[cheat_y as usize][cheat_x as usize];

                        let solusion_start = &self.solusion_cost_map[y][x];
                        let solusion_end =
                            &self.solusion_cost_map[cheat_y as usize][cheat_x as usize];

                        if map_start.cost.is_none() {
                            continue;
                        }
                        if solusion_start.cost.is_none() || solusion_end.cost.is_none() {
                            continue;
                        }

                        assert_eq!(map_start.kind, solusion_start.kind);
                        assert_eq!(map_end.kind, solusion_end.kind);

                        if TailKind::Wall == map_start.kind || TailKind::Wall == map_end.kind {
                            continue;
                        }

                        // Potential cheat found, the start and ends are are both on emtpy places

                        let cost = map_start.cost.unwrap();
                        let cost_til_solusion_start = solusion_start.cost.unwrap();
                        let cost_til_solusion_end = solusion_end.cost.unwrap();

                        if cost_til_solusion_end > cost_til_solusion_start {
                            continue;
                        }

                        if cost + (diff as usize) + cost_til_solusion_end - 1 <= minimal_cost {
                            total_cheats_p2 += 1;
                            if diff == 2 {
                                total_cheats_p1 += 1;
                            }
                        }
                    }
                }

                if TailKind::Empty == self.map[y][x].kind {
                    continue;
                }

                // This is a wall, check all cheat options
                let cheats = [
                    ((x - 1, y), (x + 1, y)),
                    ((x + 1, y), (x - 1, y)),
                    ((x, y - 1), (x, y + 1)),
                    ((x, y + 1), (x, y - 1)),
                ];

                for (start, end) in cheats {
                    let map_start = &self.map[start.1][start.0];
                    let map_end = &self.map[end.1][end.0];

                    let solusion_start = &self.solusion_cost_map[start.1][start.0];
                    let solusion_end = &self.solusion_cost_map[end.1][end.0];

                    if map_start.cost.is_none() {
                        continue;
                    }
                    if solusion_start.cost.is_none() || solusion_end.cost.is_none() {
                        continue;
                    }

                    assert_eq!(map_start.kind, solusion_start.kind);
                    assert_eq!(map_end.kind, solusion_end.kind);

                    if TailKind::Wall == map_start.kind || TailKind::Wall == map_end.kind {
                        continue;
                    }

                    // Potential cheat found, the start and ends are are both on emtpy places

                    let cost = map_start.cost.unwrap();
                    let cost_til_solusion_start = solusion_start.cost.unwrap();
                    let cost_til_solusion_end = solusion_end.cost.unwrap();

                    if cost_til_solusion_end > cost_til_solusion_start {
                        continue;
                    }

                    if cost + 2 + cost_til_solusion_end <= minimal_cost {
                        total_cheats += 1;
                    }
                }
            }
        }

        println!("{}", total_cheats);
        println!("{}", total_cheats_p1);
        println!("{}", total_cheats_p2);
    }
}
