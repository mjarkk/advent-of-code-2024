use std::fs;

struct PuzzleSize {
    width: usize,
    height: usize,
    total: usize,
}

fn main() {
    use std::time::Instant;
    let now = Instant::now();

    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut walls: Vec<bool> = Vec::with_capacity(puzzle.len());
    let mut player_location = 0;
    let mut puzzle_size = PuzzleSize {
        width: 0,
        height: 0,
        total: 0,
    };
    let mut location_flags: Vec<u8> = Vec::with_capacity(puzzle.len());
    for (line_idx, line) in puzzle.lines().enumerate() {
        puzzle_size.width = line.len();
        puzzle_size.height += 1;
        for (char_idx, c) in line.chars().enumerate() {
            location_flags.push(0);
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

    let found_out = resolve(&walls, player_location, &puzzle_size, &mut location_flags);
    if !found_out {
        panic!("No solution found");
    }

    // Place all the locations of all the flags that were visited in a new list
    let mut visited_locations: Vec<usize> = Vec::new();
    for (idx, flags) in location_flags.iter().enumerate() {
        if *flags == 0 {
            continue;
        }
        visited_locations.push(idx);
    }

    let mut answer_p1 = 0;
    let mut answer_p2 = 0;
    for idx in visited_locations {
        answer_p1 += 1;
        walls[idx] = true;

        if !resolve(&walls, player_location, &puzzle_size, &mut location_flags) {
            answer_p2 += 1;
        }

        walls[idx] = false;
    }

    println!("part1 {}", answer_p1);
    println!("part2 {}", answer_p2);

    println!("Elapsed: {:.2?}", now.elapsed());
}

const DIRECTION_UP: u8 = 0b0000_0001;
const DIRECTION_LEFT: u8 = 0b0000_0010;
const DIRECTION_DOWN: u8 = 0b0000_0100;
const DIRECTION_RIGHT: u8 = 0b0000_1000;

fn resolve(
    walls: &Vec<bool>,
    mut player_location: usize,
    puzzle_size: &PuzzleSize,
    location_flags: &mut Vec<u8>,
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
    for idx in 0..location_flags.len() {
        location_flags[idx] = 0;
    }

    location_flags[player_location] = direction;

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
        let flags = location_flags[new_player_location];
        if flags == flags | direction {
            return false;
        }
        location_flags[new_player_location] = flags | direction;
    }

    return true;
}
