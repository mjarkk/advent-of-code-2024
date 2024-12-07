struct PuzzleSize {
    width: usize,
    height: usize,
    total: usize,
}

fn main() {
    let now = std::time::Instant::now();

    let puzzle = std::fs::read_to_string("./puzzle.txt").unwrap();

    let mut walls: Vec<bool> = Vec::with_capacity(puzzle.len());
    let mut player_location = 0;
    let mut puzzle_size = PuzzleSize {
        width: 0,
        height: 0,
        total: 0,
    };

    let mut location_visit_flags: Vec<u8> = Vec::with_capacity(puzzle.len());

    let mut walls_lookups: Vec<[u8; 4]> = Vec::with_capacity(puzzle.len());

    for (line_idx, line) in puzzle.lines().enumerate() {
        puzzle_size.width = line.len();
        puzzle_size.height += 1;
        for (char_idx, c) in line.chars().enumerate() {
            location_visit_flags.push(0);
            walls_lookups.push([0, 0, 0, 0]);
            match c {
                '#' => {
                    walls.push(true);
                }
                '^' => {
                    walls.push(false);
                    player_location = line_idx * puzzle_size.width + char_idx;
                }
                _ => {
                    walls.push(false);
                }
            };
        }
    }
    puzzle_size.total = puzzle_size.width * puzzle_size.height;

    for x in 0..puzzle_size.width {
        // From top bottom
        let mut last_wall: Option<usize> = None;
        for y in 0..puzzle_size.height {
            let idx = y * puzzle_size.width + x;
            if walls[idx] {
                last_wall = Some(y);
                continue;
            }

            if let Some(last_wall) = last_wall {
                walls_lookups[idx][DIRECTION_UP as usize] = (y - last_wall - 1) as u8;
            }
        }

        // From bottom to top
        last_wall = None;
        for y in (0..puzzle_size.height).rev() {
            let idx = y * puzzle_size.width + x;
            if walls[idx] {
                last_wall = Some(y);
                continue;
            }

            if let Some(last_wall) = last_wall {
                walls_lookups[idx][DIRECTION_DOWN as usize] = (last_wall - y - 1) as u8;
            }
        }
    }

    for y in 0..puzzle_size.height {
        // From left to right
        let mut last_wall: Option<usize> = None;
        for x in 0..puzzle_size.width {
            let idx = y * puzzle_size.width + x;
            if walls[idx] {
                last_wall = Some(x);
                continue;
            }

            if let Some(last_wall) = last_wall {
                walls_lookups[idx][DIRECTION_LEFT as usize] = (x - last_wall - 1) as u8;
            }
        }

        // From right to left
        last_wall = None;
        for x in (0..puzzle_size.width).rev() {
            let idx = y * puzzle_size.width + x;
            if walls[idx] {
                last_wall = Some(x);
                continue;
            }

            if let Some(last_wall) = last_wall {
                walls_lookups[idx][DIRECTION_RIGHT as usize] = (last_wall - x - 1) as u8;
            }
        }
    }

    let found_out = resolve_p1(
        &walls,
        player_location,
        &puzzle_size,
        &mut location_visit_flags,
    );
    if !found_out {
        panic!("No solution found");
    }

    // Place all the locations of all the flags that were visited in a new list
    let mut visited_locations: Vec<usize> = Vec::new();
    for (idx, flags) in location_visit_flags.iter().enumerate() {
        if *flags == 0 {
            continue;
        }
        visited_locations.push(idx);
    }

    let mut answer_p1 = 0;
    let mut answer_p2 = 0;
    for idx in visited_locations {
        answer_p1 += 1;

        if idx == player_location {
            continue;
        }

        walls[idx] = true;

        if !resolve_p2(
            &walls,
            player_location,
            &puzzle_size,
            &mut location_visit_flags,
            &walls_lookups,
            idx,
        ) {
            answer_p2 += 1;
        }

        walls[idx] = false;
    }

    println!("part1 {}", answer_p1);
    println!("part2 {}", answer_p2);

    println!("Elapsed: {:.2?}", now.elapsed());
}

const DIRECTION_UP: u8 = 0;
const DIRECTION_LEFT: u8 = 1;
const DIRECTION_DOWN: u8 = 2;
const DIRECTION_RIGHT: u8 = 3;

fn resolve_p1(
    walls: &Vec<bool>,
    mut player_location: usize,
    puzzle_size: &PuzzleSize,
    location_visit_flags: &mut Vec<u8>,
) -> bool {
    let directions: [u8; 4] = [
        DIRECTION_RIGHT,
        DIRECTION_DOWN,
        DIRECTION_LEFT,
        DIRECTION_UP,
    ];
    let mut direction_idx = 3;
    let mut direction = directions[direction_idx];

    location_visit_flags[player_location] = 1 << direction;

    loop {
        let new_player_location = match direction {
            DIRECTION_UP => {
                let y = player_location / puzzle_size.width;
                if y == 0 {
                    break;
                }
                player_location - puzzle_size.width
            }
            DIRECTION_LEFT => {
                let x = player_location % puzzle_size.width;
                if x == 0 {
                    break;
                }
                player_location - 1
            }
            DIRECTION_DOWN => {
                let y = player_location / puzzle_size.width;
                if y == puzzle_size.height - 1 {
                    break;
                }
                player_location + puzzle_size.width
            }
            DIRECTION_RIGHT => {
                let x = player_location % puzzle_size.width;
                if x == puzzle_size.width - 1 {
                    break;
                }
                player_location + 1
            }
            _ => {
                break;
            }
        };

        if walls[new_player_location] {
            direction_idx = (direction_idx + 1) % 4;
            direction = directions[direction_idx];
            continue;
        }

        player_location = new_player_location;
        location_visit_flags[new_player_location] = 1;
    }

    return true;
}

fn resolve_p2(
    walls: &Vec<bool>,
    mut player_location: usize,
    puzzle_size: &PuzzleSize,
    location_visit_flags: &mut Vec<u8>,
    walls_lookups: &Vec<[u8; 4]>,
    walls_lookup_skip_axis: usize,
) -> bool {
    let directions: [u8; 4] = [
        DIRECTION_RIGHT,
        DIRECTION_DOWN,
        DIRECTION_LEFT,
        DIRECTION_UP,
    ];
    let mut direction_idx = 3;
    let mut direction = directions[direction_idx];

    // Reset all the flags
    for idx in 0..location_visit_flags.len() {
        location_visit_flags[idx] = 0;
    }

    let skip_x = walls_lookup_skip_axis % puzzle_size.width;
    let skip_y = walls_lookup_skip_axis / puzzle_size.width;
    let walls_lookup_skip_axis = (skip_x, skip_y);

    loop {
        let y = player_location / puzzle_size.width;
        let x = player_location % puzzle_size.width;
        let mut new_player_location = match direction {
            DIRECTION_UP => {
                if y == 0 {
                    break;
                }
                player_location - puzzle_size.width
            }
            DIRECTION_LEFT => {
                if x == 0 {
                    break;
                }
                player_location - 1
            }
            DIRECTION_DOWN => {
                if y == puzzle_size.height - 1 {
                    break;
                }
                player_location + puzzle_size.width
            }
            DIRECTION_RIGHT => {
                if x == puzzle_size.width - 1 {
                    break;
                }
                player_location + 1
            }
            _ => {
                break;
            }
        };

        if walls[new_player_location] {
            direction_idx = (direction_idx + 1) % 4;
            direction = directions[direction_idx];
            continue;
        }

        let (skip_x, skip_y) = walls_lookup_skip_axis;
        if skip_y != y && skip_x != x {
            let next_walls = walls_lookups[new_player_location][direction as usize];
            if next_walls > 0 {
                new_player_location = match direction {
                    DIRECTION_UP => new_player_location - (puzzle_size.width * next_walls as usize),
                    DIRECTION_LEFT => new_player_location - next_walls as usize,
                    DIRECTION_DOWN => {
                        new_player_location + (puzzle_size.width * next_walls as usize)
                    }
                    DIRECTION_RIGHT => new_player_location + next_walls as usize,
                    _ => new_player_location,
                };
            }
        }

        player_location = new_player_location;
        let flags = location_visit_flags[new_player_location];
        let direction_flag = 1 << direction;
        if flags == flags | direction_flag {
            return false;
        }
        location_visit_flags[new_player_location] = flags | direction_flag;
    }

    return true;
}
