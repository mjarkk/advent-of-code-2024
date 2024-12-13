use std::fs;
use std::time::Instant;

struct Machine {
    a: (isize, isize),
    b: (isize, isize),
    prize: (isize, isize),
}

impl Default for Machine {
    fn default() -> Self {
        Machine {
            a: (0, 0),
            b: (0, 0),
            prize: (0, 0),
        }
    }
}

fn main() {
    let now = Instant::now();
    let input = fs::read_to_string("./puzzle.txt").unwrap();

    let mut machines: Vec<Machine> = Vec::new();
    for block in input.split("\n\n") {
        let mut puzzle = Machine::default();
        let mut has_answer = false;
        for (idx, line) in block.lines().enumerate() {
            if idx < 2 {
                let x = &line[12..14];
                let y = &line[18..20];
                let cords: (isize, isize) = (x.parse().unwrap(), y.parse().unwrap());
                if idx == 0 {
                    puzzle.a = cords;
                } else {
                    puzzle.b = cords;
                }
                continue;
            }

            let mut parts = line.split('=');
            parts.next();
            let mut x_str = parts.next().unwrap();
            let y_str = parts.next().unwrap();
            x_str = x_str.split(',').next().unwrap();

            puzzle.prize = (x_str.parse().unwrap(), y_str.trim().parse().unwrap());
            has_answer = true;
        }

        if !has_answer {
            continue;
        }

        machines.push(puzzle);
    }

    let mut result_p1 = 0isize;
    for machine in machines.iter() {
        if let Some(price) = machine.solve(0) {
            result_p1 += price;
        }
    }
    println!("{}", result_p1);

    let mut result_p2 = 0isize;
    for machine in machines.iter() {
        if let Some(price) = machine.solve(10000000000000) {
            result_p2 += price;
        }
    }

    println!("{}", result_p2);

    println!("Elapsed: {:.2?}", now.elapsed());
}

impl Machine {
    fn solve(&self, offset: isize) -> Option<isize> {
        let prize = (self.prize.0 + offset, self.prize.1 + offset);
        let det = self.a.0 * self.b.1 - self.a.1 * self.b.0;

        let a_multiplier = (prize.0 * self.b.1 - prize.1 * self.b.0) / det;
        let b_multiplier = (self.a.0 * prize.1 - self.a.1 * prize.0) / det;

        let a_answer = self.a.0 * a_multiplier + self.b.0 * b_multiplier;
        let b_answer = self.a.1 * a_multiplier + self.b.1 * b_multiplier;

        if (a_answer, b_answer) == (prize.0, prize.1) {
            Some(a_multiplier * 3 + b_multiplier)
        } else {
            None
        }
    }
}
