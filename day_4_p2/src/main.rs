use std::fs;

fn main() {
    let contents = fs::read_to_string("./puzzle.txt").unwrap();

    let mut lines: Vec<Vec<char>> = Vec::new();
    for line in contents.lines() {
        lines.push(line.chars().collect());
    }

    let state = State { lines };

    let mut total = 0;
    for x in 0..state.max_x() {
        for y in 0..state.max_y() {
            if state.check_cord_for_win(x, y).is_some() {
                total += 1;
            }
        }
    }

    println!("{}", total);
}

struct State {
    lines: Vec<Vec<char>>,
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

    fn check_cord_for_win(&self, x: isize, y: isize) -> Option<()> {
        let start = self.get(x, y)?;
        if start != 'A' {
            return None;
        }

        let top_left = self.get(x - 1, y - 1)?;
        let top_right = self.get(x + 1, y - 1)?;
        let bottom_left = self.get(x - 1, y + 1)?;
        let bottom_right = self.get(x + 1, y + 1)?;

        match (top_left, bottom_right, top_right, bottom_left) {
            ('M', 'S', 'M', 'S') => Some(()),
            ('S', 'M', 'S', 'M') => Some(()),
            ('M', 'S', 'S', 'M') => Some(()),
            ('S', 'M', 'M', 'S') => Some(()),
            _ => None,
        }
    }
}
