use std::collections::HashMap;
use std::fs;
use std::time::Instant;

struct LabelResults {
    tails: usize,
    fenses: usize,
    corners_top_left: usize,
    corners_top_right: usize,
    corners_bottom_left: usize,
    corners_bottom_right: usize,
}

impl LabelResults {
    fn combine(mut self, b: Self) -> Self {
        self.tails += b.tails;
        self.fenses += b.fenses;
        self.corners_top_left += b.corners_top_left;
        self.corners_top_right += b.corners_top_right;
        self.corners_bottom_left += b.corners_bottom_left;
        self.corners_bottom_right += b.corners_bottom_right;
        self
    }
}

#[derive(Clone, Copy)]
struct Fense {
    top: bool,
    right: bool,
    bottom: bool,
    left: bool,
}

#[derive(Clone, Copy)]
struct FenseCounter {
    tails: usize,
    crop: char,
    top: usize,
    right: usize,
    bottom: usize,
    left: usize,
}

impl Fense {
    fn count(&self) -> LabelResults {
        let mut count = 0;
        if self.top {
            count += 1;
        }
        if self.right {
            count += 1;
        }
        if self.bottom {
            count += 1;
        }
        if self.left {
            count += 1;
        }
        LabelResults {
            tails: 1,
            fenses: count,
            corners_top_left: if self.top && self.left { 1 } else { 0 },
            corners_top_right: if self.top && self.right { 1 } else { 0 },
            corners_bottom_left: if self.bottom && self.left { 1 } else { 0 },
            corners_bottom_right: if self.bottom && self.right { 1 } else { 0 },
        }
    }
}

enum Direction {
    Top,
    Right,
    Bottom,
    Left,
}

#[derive(Clone, Copy)]
struct Tail {
    id: isize,
    crop: char,
    fense: Fense,
}

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut solver = Solver::new();
    for line in puzzle.lines() {
        if line.is_empty() {
            continue;
        }

        let mut line_chars = Vec::new();
        for c in line.chars() {
            line_chars.push(c);
        }
        solver.map.push(line_chars);
    }
    solver.map_size = (solver.map[0].len(), solver.map.len());

    println!("Loading puzzle input:\t{:.2?}", now.elapsed());
    let now = Instant::now();

    solver.add_fenses();

    println!("Adding fenses:\t{:.2?}", now.elapsed());
    let now = Instant::now();

    let (result_p1, result_p2) = solver.label_unique_areas_and_calculate_result_p1();
    println!("p1: {}", result_p1);
    println!("p2: {}", result_p2);

    println!("Solved part:\t{:.2?}", now.elapsed());
    let now = Instant::now();

    let mut all_sides: HashMap<isize, FenseCounter> = HashMap::new();

    for y in 0..solver.map_size.1 {
        for x in 0..solver.map_size.0 {
            let tail = solver.tails[y][x];

            let mut new_fenses = FenseCounter {
                tails: 1,
                crop: tail.crop,
                top: 0,
                left: 0,
                bottom: 0,
                right: 0,
            };
            if x == 0 && y == 0 {
                new_fenses.top = tail.fense.top as usize;
                new_fenses.left = tail.fense.left as usize;
                new_fenses.bottom = tail.fense.bottom as usize;
                new_fenses.right = tail.fense.right as usize;
            } else {
                if x != 0 && (tail.fense.top || tail.fense.bottom) {
                    let prev_tail = solver.tails[y][x - 1];
                    if prev_tail.id == tail.id {
                        if tail.fense.top && !prev_tail.fense.top {
                            new_fenses.top = 1;
                        }
                        if tail.fense.bottom && !prev_tail.fense.bottom {
                            new_fenses.bottom = 1;
                        }
                    } else {
                        new_fenses.top = tail.fense.top as usize;
                        new_fenses.bottom = tail.fense.bottom as usize;
                    }
                } else {
                    new_fenses.top = tail.fense.top as usize;
                    new_fenses.bottom = tail.fense.bottom as usize;
                }

                if y != 0 && (tail.fense.left || tail.fense.right) {
                    let prev_tail = solver.tails[y - 1][x];
                    if prev_tail.id == tail.id {
                        if tail.fense.left && !prev_tail.fense.left {
                            new_fenses.left = 1;
                        }
                        if tail.fense.right && !prev_tail.fense.right {
                            new_fenses.right = 1;
                        }
                    } else {
                        new_fenses.left = tail.fense.left as usize;
                        new_fenses.right = tail.fense.right as usize;
                    }
                } else {
                    new_fenses.left = tail.fense.left as usize;
                    new_fenses.right = tail.fense.right as usize;
                }
            }

            match all_sides.get_mut(&tail.id) {
                None => {
                    all_sides.insert(tail.id, new_fenses);
                }
                Some(v) => {
                    v.tails += new_fenses.tails;
                    assert_eq!(v.crop, new_fenses.crop);
                    v.top += new_fenses.top;
                    v.right += new_fenses.right;
                    v.bottom += new_fenses.bottom;
                    v.left += new_fenses.left;
                }
            }
        }
    }

    let mut result = 0;
    for (_, tail) in all_sides.iter() {
        if tail.left == 0 {
            panic!("Left side is 0 for crop {}", tail.crop);
        }
        if tail.right == 0 {
            panic!("Right side is 0 for crop {}", tail.crop);
        }
        if tail.top == 0 {
            panic!("Top side is 0 for crop {}", tail.crop);
        }
        if tail.bottom == 0 {
            panic!("Bottom side is 0 for crop {}", tail.crop);
        }
        let total_sides = tail.top + tail.right + tail.bottom + tail.left;
        result += total_sides * tail.tails;
    }

    println!("p2: {}", result);

    println!("Elapsed: {:.2?}", now.elapsed());
}

struct Solver {
    map: Vec<Vec<char>>,
    map_size: (usize, usize),
    tails: Vec<Vec<Tail>>,
}

impl Solver {
    fn new() -> Self {
        Self {
            map: Vec::new(),
            map_size: (0, 0),
            tails: Vec::new(),
        }
    }

    // Calculate for every tail the fenses around it
    fn add_fenses(&mut self) {
        for y in 0..self.map_size.1 {
            let mut row_tails: Vec<Tail> = Vec::new();
            for x in 0..self.map_size.0 {
                let crop = self.map[y][x];
                let mut tail = Tail {
                    id: -1,
                    crop,
                    fense: Fense {
                        top: false,
                        right: false,
                        bottom: false,
                        left: false,
                    },
                };
                if y == 0 {
                    tail.fense.top = true;
                } else if self.map[y - 1][x] != crop {
                    tail.fense.top = true;
                    self.tails[y - 1][x].fense.bottom = true;
                }
                if y == self.map_size.1 - 1 {
                    tail.fense.bottom = true;
                }

                if x == 0 {
                    tail.fense.left = true;
                } else if self.map[y][x - 1] != crop {
                    tail.fense.left = true;
                    row_tails[x - 1].fense.right = true;
                }
                if x == self.map_size.0 - 1 {
                    tail.fense.right = true;
                }

                row_tails.push(tail);
            }

            self.tails.push(row_tails);
        }
    }

    // Give every unique area a unique id for later use and calculate the result for part 1
    fn label_unique_areas_and_calculate_result_p1(&mut self) -> (usize, usize) {
        let mut crop_id_counter = 0isize;
        let mut result_p1 = 0;
        let mut result_p2 = 0;

        for y in 0..self.map_size.1 {
            for x in 0..self.map_size.0 {
                let tail = self.tails[y][x];
                if tail.id != -1 {
                    continue;
                }

                if let Some(area_results) = self.label_nabors(crop_id_counter, tail.crop, (x, y)) {
                    crop_id_counter += 1;

                    result_p1 += area_results.tails * area_results.fenses;

                    let bottom_left = area_results.corners_bottom_left * 2 - 1;
                    let bottom_right = area_results.corners_bottom_right * 2 - 1;
                    let top_left = area_results.corners_top_left * 2 - 1;
                    let top_right = area_results.corners_top_right * 2 - 1;

                    result_p2 +=
                        area_results.tails * (top_right + top_left + bottom_right + bottom_left);
                }
            }
        }

        (result_p1, result_p2)
    }

    // label_nabors is a recursive function that labels all the same crops that are connected to the current tail
    // As this is recursive it will keep doing this till all tails are labeled
    fn label_nabors(
        &mut self,
        id: isize,
        crop_name: char,
        cord: (usize, usize),
    ) -> Option<LabelResults> {
        let mut tail = self.tails[cord.1][cord.0];
        if tail.crop != crop_name || tail.id != -1 {
            return None;
        }

        tail.id = id;
        self.tails[cord.1][cord.0] = tail;

        let directions = [
            Direction::Top,
            Direction::Right,
            Direction::Bottom,
            Direction::Left,
        ];

        let mut results = tail.fense.count();

        for direction in directions.iter() {
            let mut new_cord = cord;
            match direction {
                Direction::Top => {
                    if tail.fense.top || cord.1 == 0 {
                        continue;
                    }
                    new_cord.1 -= 1;
                }
                Direction::Right => {
                    if tail.fense.right || cord.0 == self.map_size.0 - 1 {
                        continue;
                    }
                    new_cord.0 += 1;
                }
                Direction::Bottom => {
                    if tail.fense.bottom || cord.1 == self.map_size.1 - 1 {
                        continue;
                    }
                    new_cord.1 += 1;
                }
                Direction::Left => {
                    if tail.fense.left || cord.0 == 0 {
                        continue;
                    }
                    new_cord.0 -= 1;
                }
            }

            if let Some(tail_results) = self.label_nabors(id, crop_name, new_cord) {
                results = results.combine(tail_results);
            }
        }

        Some(results)
    }
}
