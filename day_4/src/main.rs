use std::fs;

fn main() {
    let contents = fs::read_to_string("./puzzle.txt").unwrap();

    let mut lines: Vec<Vec<char>> = Vec::new();
    for line in contents.lines() {
        lines.push(line.chars().collect());
    }

    let looking_for: Vec<char> = "XMAS".chars().collect();
    let mut looking_for_reverse = looking_for.clone();
    looking_for_reverse.reverse();

    let state = State {
        lines,
        cords_to_check: vec![
            (1, 0),  // To the right
            (1, 1),  // Diagonal down right
            (0, 1),  // Down
            (-1, 1), // Diagonal down left
        ],
        looking_for,
        looking_for_reverse,
    };

    let mut total = 0;
    for x in 0..state.max_x() {
        for y in 0..state.max_y() {
            if let Some(v) = state.check_cord_for_win(x, y) {
                total += v;
            }
        }
    }

    println!("{}", total);
}

struct State {
    lines: Vec<Vec<char>>,
    cords_to_check: Vec<(isize, isize)>,
    looking_for: Vec<char>,
    looking_for_reverse: Vec<char>,
}

impl State {
    fn max_y(&self) -> isize {
        self.lines.len() as isize
    }
    fn max_x(&self) -> isize {
        self.lines
            .iter()
            .map(|line| line.len() as isize)
            .max()
            .unwrap()
    }

    fn get(&self, x: isize, y: isize) -> Option<char> {
        if x < 0 || y < 0 {
            return None;
        }

        let line = self.lines.get(y as usize)?;
        let char = line.get(x as usize)?;
        Some(*char)
    }

    fn check_cord_for_win(&self, x: isize, y: isize) -> Option<usize> {
        let start = self.get(x, y)?;

        let search_needle = if start == self.looking_for[0] {
            &self.looking_for
        } else if start == self.looking_for_reverse[0] {
            &self.looking_for_reverse
        } else {
            return None;
        };

        let mut found = 0;

        'outer: for (dx, dy) in self.cords_to_check.iter() {
            for remainder in 1..search_needle.len() {
                let x_to_check = x + (dx * remainder as isize);
                let y_to_check = y + (dy * remainder as isize);

                let next = match self.get(x_to_check, y_to_check) {
                    Some(c) => c,
                    None => continue 'outer,
                };

                if next != search_needle[remainder] {
                    continue 'outer;
                }
            }

            found += 1;
        }

        Some(found)
    }
}
