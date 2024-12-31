pub enum Pad {
    Numpad,
    Dirpad,
}

impl Pad {
    pub fn position(&self, c: char) -> (i8, i8) {
        match (self, c) {
            // Numpad
            (Self::Numpad, '7') => (0, 0),
            (Self::Numpad, '8') => (1, 0),
            (Self::Numpad, '9') => (2, 0),

            (Self::Numpad, '4') => (0, 1),
            (Self::Numpad, '5') => (1, 1),
            (Self::Numpad, '6') => (2, 1),

            (Self::Numpad, '1') => (0, 2),
            (Self::Numpad, '2') => (1, 2),
            (Self::Numpad, '3') => (2, 2),

            (Self::Numpad, '0') => (1, 3),
            (Self::Numpad, 'A') => (2, 3),

            // Dirpad
            (Self::Dirpad, '^') => (1, 0),
            (Self::Dirpad, 'A') => (2, 0),

            (Self::Dirpad, '<') => (0, 1),
            (Self::Dirpad, 'v') => (1, 1),
            (Self::Dirpad, '>') => (2, 1),

            _ => panic!("Unknown position for {}", c),
        }
    }

    pub fn invalid_position(&self) -> (i8, i8) {
        match self {
            Self::Numpad => (0, 3),
            Self::Dirpad => (0, 0),
        }
    }

    pub fn calculate_from_to(&self, from: char, to: char, resp: &mut Vec<char>) {
        let (x1, y1) = self.position(from);
        let (x2, y2) = self.position(to);

        let invalid = self.invalid_position();
        if invalid == (x1, y2) || invalid == (x2, y1) {
            // The default path would hit the invalid position here so we add the positions in reverse
            for _ in 0..x2 - x1 {
                resp.push('>');
            }
            for _ in 0..y1 - y2 {
                resp.push('^');
            }
            for _ in 0..y2 - y1 {
                resp.push('v');
            }
            for _ in 0..x1 - x2 {
                resp.push('<');
            }
        } else {
            for _ in 0..x1 - x2 {
                resp.push('<');
            }
            for _ in 0..y2 - y1 {
                resp.push('v');
            }
            for _ in 0..y1 - y2 {
                resp.push('^');
            }
            for _ in 0..x2 - x1 {
                resp.push('>');
            }
        }

        resp.push('A');
    }

    pub fn calculate_moves(&self, to_press: &Vec<char>) -> Vec<char> {
        let mut resp = Vec::new();

        let mut prev = 'A';
        for key in to_press {
            self.calculate_from_to(prev, *key, &mut resp);
            prev = *key;
        }

        resp
    }
}

fn cache_base(c: char) -> usize {
    match c {
        '^' => 0,
        'A' => 1,
        '<' => 2,
        'v' => 3,
        '>' => 4,
        c => panic!("Unknown cache base for {}", c),
    }
}

fn cache_nr(from: char, to: char) -> usize {
    cache_base(from) * 5 + cache_base(to)
}

pub struct PadSolver {
    caches: Vec<[Option<usize>; 25]>,
}

impl PadSolver {
    pub fn new() -> Self {
        Self { caches: Vec::new() }
    }
    pub fn solve_recursive(&mut self, actions: &Vec<char>, n: usize) -> usize {
        if n == 0 {
            return 1;
        }

        if self.caches.len() == 0 {
            for _ in 0..26 {
                self.caches.push([None; 25]);
            }
        }

        let mut resp = 0;
        let mut new_actions: Vec<char> = Vec::new();
        let mut prev = 'A';
        for action in actions {
            let cache_key = cache_nr(prev, *action);
            if let Some(cached_cost) = self.caches[n - 1][cache_key] {
                resp += cached_cost;
                prev = *action;
                continue;
            }

            new_actions.clear();
            Pad::Dirpad.calculate_from_to(prev, *action, &mut new_actions);
            let cost = self.solve_recursive(&new_actions, n - 1);

            self.caches[n - 1][cache_key] = Some(cost);

            resp += cost;
            prev = *action;
        }

        return resp;
    }
}
