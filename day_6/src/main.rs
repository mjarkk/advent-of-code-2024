use std::{collections::HashSet, fs};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Cord {
    x: i16,
    y: i16,
}

impl Cord {
    fn with_offset(&self, offset: &Self) -> Self {
        Self {
            x: self.x + offset.x,
            y: self.y + offset.y,
        }
    }
}

fn main() {
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut walls: HashSet<Cord> = HashSet::new();
    let mut player_location = Cord { x: 0, y: 0 };

    let mut puzzle_width = 0;
    let mut puzzle_height = 0;
    for (line_idx, line) in puzzle.lines().enumerate() {
        puzzle_width = line.len() as i16;
        puzzle_height += 1;
        for (char_idx, c) in line.chars().enumerate() {
            match c {
                '#' => {
                    walls.insert(Cord {
                        y: line_idx as i16,
                        x: char_idx as i16,
                    });
                }
                '^' => {
                    player_location = Cord {
                        y: line_idx as i16,
                        x: char_idx as i16,
                    };
                }
                _ => {}
            };
        }
    }

    let visited_locations = resolve(
        walls.clone(),
        player_location.clone(),
        puzzle_height,
        puzzle_width,
    )
    .unwrap();

    println!("part1 {}", visited_locations.len());

    let mut answer_p2 = 0;
    for visited_location in visited_locations.clone() {
        let mut walls_with_paradox = walls.clone();
        walls_with_paradox.insert(visited_location);

        if resolve(
            walls_with_paradox,
            player_location.clone(),
            puzzle_height,
            puzzle_width,
        )
        .is_none()
        {
            answer_p2 += 1;
        }
    }

    println!("part2 {}", answer_p2);
}

fn resolve(
    walls: HashSet<Cord>,
    mut player_location: Cord,
    puzzle_height: i16,
    puzzle_width: i16,
) -> Option<HashSet<Cord>> {
    let directions: [Cord; 4] = [
        Cord { x: 1, y: 0 },  // right
        Cord { x: 0, y: 1 },  // down
        Cord { x: -1, y: 0 }, // left
        Cord { x: 0, y: -1 }, // up
    ];
    let mut direction = Cord { x: 0, y: -1 };
    let mut direction_idx = 3;

    let mut visited_locations: HashSet<Cord> = HashSet::new();
    visited_locations.insert(player_location);
    let mut iter = 0;
    loop {
        iter += 1;
        if iter > 10_000 {
            return None;
        }

        let new_player_location = player_location.with_offset(&direction);
        if new_player_location.x < 0
            || new_player_location.y < 0
            || new_player_location.x >= puzzle_width
            || new_player_location.y >= puzzle_height
        {
            break;
        }

        if walls.contains(&new_player_location) {
            direction_idx = (direction_idx + 1) % 4;
            direction = directions[direction_idx];
            continue;
        }

        player_location = new_player_location;
        visited_locations.insert(player_location);
    }

    Some(visited_locations)
}
