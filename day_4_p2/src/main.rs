use std::fs;

fn main() {
    let contents = fs::read_to_string("./puzzle.txt").unwrap();

    let mut lines: Vec<Vec<char>> = Vec::new();
    for line in contents.lines() {
        lines.push(line.chars().collect());
    }

    let state = State { lines };

    let mut total = 0;
    for x in 1..state.max_x() - 1 {
        for y in 1..state.max_y() - 1 {
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
        if top_left != 'M' && top_left != 'S' {
            return None;
        }
        let bottom_right = self.get(x + 1, y + 1)?;
        if bottom_right != 'M' && bottom_right != 'S' {
            return None;
        }
        if top_left == bottom_right {
            return None;
        }

        let top_right = self.get(x + 1, y - 1)?;
        if top_right != 'M' && top_right != 'S' {
            return None;
        }
        let bottom_left = self.get(x - 1, y + 1)?;
        if bottom_left != 'M' && bottom_left != 'S' {
            return None;
        }
        if top_right == bottom_left {
            return None;
        }

        Some(())
    }
}
