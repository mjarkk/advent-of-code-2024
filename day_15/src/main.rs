use std::fs;
use std::time::Instant;

#[derive(Debug, Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Into<Direction> for char {
    fn into(self) -> Direction {
        match self {
            '^' => Direction::Up,
            'v' => Direction::Down,
            '<' => Direction::Left,
            '>' => Direction::Right,
            _ => panic!("Invalid direction"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Side {
    Left,
    Right,
}

impl Side {
    fn opposite(&self) -> Side {
        match self {
            Side::Left => Side::Right,
            Side::Right => Side::Left,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Tail {
    Empty,
    Wall,
    Box(Side),
}

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut state = State {
        double_width: false,
        directions: Vec::new(),
        robot_position: (0, 0),
        map_size: (0, 0),
        map: Vec::new(),
    };

    for line in puzzle.lines() {
        let mut chars = line.chars();
        match chars.next() {
            Some('#') => {
                // Parse part of map
                let mut row = Vec::new();
                let mut x = 0;
                let mut non_walls = 0;
                for char in chars {
                    if char == '@' {
                        state.robot_position = (state.map.len(), x);
                    }

                    x += 1;

                    row.push(match char {
                        '#' => Tail::Wall,
                        '.' | '@' => {
                            non_walls += 1;
                            Tail::Empty
                        }
                        'O' => {
                            non_walls += 1;
                            Tail::Box(Side::Left)
                        }
                        _ => panic!("invalid character"),
                    });
                }

                if non_walls > 0 {
                    row.pop();
                    state.map.push(row);
                }
            }
            Some(char) => {
                state.directions.push(char.into());
                for char in chars {
                    state.directions.push(char.into());
                }
            }
            None => continue,
        }
    }

    state.map_size = (state.map[0].len(), state.map.len());
    let mut part_2_state = state.clone();

    state.move_robot();

    part_2_state.convert_to_double_width();
    part_2_state.move_robot();

    println!("{}", state.calculate_score());
    println!("{}", part_2_state.calculate_score());
    println!("Elapsed: {:.2?}", now.elapsed());
}

#[derive(Debug, Clone)]
struct State {
    directions: Vec<Direction>,
    robot_position: (usize, usize),
    map_size: (usize, usize),
    map: Vec<Vec<Tail>>,
    double_width: bool,
}

impl State {
    fn _debug_print_map(&self) {
        for y in 0..self.map_size.1 {
            for x in 0..self.map_size.0 {
                if (x, y) == self.robot_position {
                    print!("@");
                    continue;
                }

                print!(
                    "{}",
                    match self.map[y][x] {
                        Tail::Empty => '.',
                        Tail::Wall => '#',
                        Tail::Box(_) if !self.double_width => 'O',
                        Tail::Box(Side::Left) => '[',
                        Tail::Box(Side::Right) => ']',
                    }
                );
            }
            println!();
        }
        println!();
    }

    fn convert_to_double_width(&mut self) {
        self.double_width = true;
        let mut new_map = Vec::new();
        for y in 0..self.map_size.1 {
            let mut new_row = Vec::new();
            for x in 0..self.map_size.0 {
                let tail = self.map[y][x];
                match tail {
                    Tail::Empty => {
                        new_row.push(Tail::Empty);
                        new_row.push(Tail::Empty);
                    }
                    Tail::Wall => {
                        new_row.push(Tail::Wall);
                        new_row.push(Tail::Wall);
                    }
                    Tail::Box(_) => {
                        new_row.push(Tail::Box(Side::Left));
                        new_row.push(Tail::Box(Side::Right));
                    }
                }
            }
            new_map.push(new_row);
        }
        self.map_size = (self.map_size.0 * 2, self.map_size.1);
        self.robot_position = (self.robot_position.0 * 2, self.robot_position.1);
        self.map = new_map;
    }

    fn calculate_score(&self) -> usize {
        let mut score = 0;
        let x_offset = if self.double_width { 2 } else { 1 };
        for y in 0..self.map_size.1 {
            for x in 0..self.map_size.0 {
                match self.map[y][x] {
                    Tail::Box(Side::Left) => {
                        score += 100 * (y + 1) + x + x_offset;
                    }
                    _ => continue,
                }
            }
        }
        score
    }

    fn move_robot(&mut self) {
        let directions = self.directions.clone();
        for direction in directions {
            self.move_step(&direction);
        }
        // self._debug_print_map();
    }

    fn resolve_direction(
        &self,
        cord: (usize, usize),
        direction: &Direction,
    ) -> Option<(usize, usize)> {
        match direction {
            Direction::Up => {
                if cord.1 == 0 {
                    None
                } else {
                    Some((cord.0, cord.1 - 1))
                }
            }
            Direction::Down => {
                if cord.1 == self.map_size.1 - 1 {
                    None
                } else {
                    Some((cord.0, cord.1 + 1))
                }
            }
            Direction::Left => {
                if cord.0 == 0 {
                    None
                } else {
                    Some((cord.0 - 1, cord.1))
                }
            }
            Direction::Right => {
                if cord.0 == self.map_size.0 - 1 {
                    None
                } else {
                    Some((cord.0 + 1, cord.1))
                }
            }
        }
    }

    fn move_step(&mut self, direction: &Direction) {
        let new_pos = match self.resolve_direction(self.robot_position, direction) {
            Some(pos) => pos,
            None => return, // Nothing to move we are against the wal
        };

        if self.double_width {
            if !self.might_move_double_width_box(new_pos, direction, true) {
                return;
            }

            self.might_move_double_width_box(new_pos, direction, false);
            self.robot_position = new_pos;
            return;
        }

        if self.might_move_box(new_pos, direction) {
            self.robot_position = new_pos;
        }
    }

    fn might_move_box(&mut self, position: (usize, usize), direction: &Direction) -> bool {
        let tail = &self.map[position.1][position.0];
        match tail {
            Tail::Empty => return true,
            Tail::Wall => return false, // we cannot move a wall
            Tail::Box(_) => {}          // continue
        };

        let new_pos = match self.resolve_direction(position, direction) {
            Some(pos) => pos,
            None => return false, // This box is against a wall we cannot more it
        };

        if !match self.map[new_pos.1][new_pos.0] {
            Tail::Empty => true,
            Tail::Wall => false, // We are trying to move the box against a wall
            Tail::Box(_) => self.might_move_box(new_pos, direction),
        } {
            return false;
        }

        self.map[position.1][position.0] = Tail::Empty;
        self.map[new_pos.1][new_pos.0] = Tail::Box(Side::Left);

        true
    }

    fn might_move_double_width_box(
        &mut self,
        position: (usize, usize),
        direction: &Direction,
        dry: bool,
    ) -> bool {
        let tail = &self.map[position.1][position.0];
        let box_side = *match tail {
            Tail::Empty => return true,
            Tail::Wall => return false, // we cannot move a wall
            Tail::Box(side) => side,    // continue
        };

        match direction {
            Direction::Left | Direction::Right => {
                // Check if we can move the box to the left or right

                // Firstly lets check if the other side of the box is present, if not something is going wrong as all boxes should be 2 tails wide
                let other_side_of_box = match self.resolve_direction(position, direction) {
                    Some(pos) => pos,
                    None => return false, // This box is against a wall we cannot more it
                };
                if let Tail::Box(other_side) = self.map[other_side_of_box.1][other_side_of_box.0] {
                    assert_ne!(other_side, box_side);
                } else {
                    panic!("box without another side found");
                }

                // Now we need to check if there is space beside the box
                let beside_box = match self.resolve_direction(other_side_of_box, direction) {
                    Some(pos) => pos,
                    None => return false, // This box is against a wall we cannot more it
                };

                if !match self.map[beside_box.1][beside_box.0] {
                    Tail::Empty => true, // We can move the box
                    Tail::Wall => false, // We are trying to move the box against a wall
                    Tail::Box(_) => {
                        // We are trying to move the box against another box, check if we can move that box
                        self.might_move_double_width_box(beside_box, direction, dry)
                    }
                } {
                    return false;
                }

                if !dry {
                    // Apply the changes
                    self.map[position.1][position.0] = Tail::Empty;
                    self.map[other_side_of_box.1][other_side_of_box.0] = Tail::Box(box_side);
                    self.map[beside_box.1][beside_box.0] = Tail::Box(box_side.opposite());
                }
            }
            Direction::Up | Direction::Down => {
                // Check if we can move the box to the top or bottom

                let other_side_of_box = match box_side {
                    Side::Left => (position.0 + 1, position.1),
                    Side::Right => (position.0 - 1, position.1),
                };

                for cord in [position, other_side_of_box] {
                    let beside_box = match self.resolve_direction(cord, direction) {
                        Some(pos) => pos,
                        None => return false, // This box is against a wall we cannot more it
                    };

                    if !match self.map[beside_box.1][beside_box.0] {
                        Tail::Empty => true, // We can move the box
                        Tail::Wall => false, // We are trying to move the box against a wall
                        Tail::Box(_) => {
                            // We are trying to move the box against another box, check if we can move that box
                            self.might_move_double_width_box(beside_box, direction, dry)
                        }
                    } {
                        return false;
                    }
                }

                if !dry {
                    // Move the current box
                    self.map[position.1][position.0] = Tail::Empty;
                    self.map[other_side_of_box.1][other_side_of_box.0] = Tail::Empty;

                    match direction {
                        Direction::Up => {
                            self.map[position.1 - 1][position.0] = Tail::Box(box_side);
                            self.map[other_side_of_box.1 - 1][other_side_of_box.0] =
                                Tail::Box(box_side.opposite());
                        }
                        Direction::Down => {
                            self.map[position.1 + 1][position.0] = Tail::Box(box_side);
                            self.map[other_side_of_box.1 + 1][other_side_of_box.0] =
                                Tail::Box(box_side.opposite());
                        }
                        _ => panic!("Invalid direction"),
                    }
                }
            }
        }

        true
    }
}
