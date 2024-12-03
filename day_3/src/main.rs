use std::fs;

fn main() {
    let puzzle: Vec<u8> = fs::read("./puzzle.txt").unwrap();

    let mut state = State::new(puzzle);

    state.looking_for_next_m();

    println!("{}", state.total);
}

const CHAR_M: u8 = 'm' as u8;
const CHAR_D: u8 = 'd' as u8;
const CHAR_N: u8 = 'n' as u8;
const CHAR_0: u8 = '0' as u8;
const CHAR_9: u8 = '9' as u8;
const CHAR_COMMA: u8 = ',' as u8;
const CHAR_OPEN_BRACKET: u8 = '(' as u8;
const CHAR_CLOSE_BRACKET: u8 = ')' as u8;

struct State {
    data: Vec<u8>,
    idx: usize,
    total: usize,
    enabled: bool,
}

impl State {
    fn new(data: Vec<u8>) -> Self {
        Self {
            data,
            idx: 0,
            total: 0,
            enabled: true,
        }
    }
    fn current(&self) -> Option<u8> {
        if self.idx >= self.data.len() {
            return None;
        }
        Some(self.data[self.idx])
    }
    fn next(&mut self) -> Option<u8> {
        self.idx += 1;
        if self.idx < self.data.len() {
            return Some(self.data[self.idx]);
        }
        None
    }
    fn looking_for_next_m(&mut self) {
        loop {
            let c = match self.current() {
                Some(c) => c,
                None => break,
            };

            let if_fails_restore_idx_to = self.idx + 1;
            match c {
                CHAR_M => {
                    // Parse muls
                    if let Some(v) = self.parse_mul() {
                        if self.enabled {
                            self.total += v;
                        }
                    } else {
                        self.idx = if_fails_restore_idx_to;
                    }
                }
                CHAR_D => {
                    // Parse "do()" and "don't()"
                    if let Some(v) = self.parse_do_dont() {
                        self.enabled = v;
                    } else {
                        self.idx = if_fails_restore_idx_to;
                    }
                }
                _ => {
                    self.next();
                }
            }
        }
    }
    fn parse_mul(&mut self) -> Option<usize> {
        self.parse_text("mul(")?;

        // Parse the left hand number
        let left_hand = self.parse_mul_number()?;

        // Parse the comma
        if self.current()? != CHAR_COMMA {
            return None;
        }

        // Parse the right hand number
        self.next()?;
        let right_hand = self.parse_mul_number()?;

        // Parse the closing bracket
        if self.current()? != CHAR_CLOSE_BRACKET {
            return None;
        }

        self.next()?;

        Some(left_hand * right_hand)
    }
    fn parse_mul_number(&mut self) -> Option<usize> {
        let mut number_chars: Vec<u8> = Vec::new();

        for idx in 0..3 {
            let c = self.current()?;
            if c < CHAR_0 || c > CHAR_9 {
                if idx == 0 {
                    return None;
                } else {
                    break;
                }
            }

            number_chars.push(c);
            self.next();
        }

        // let number = number_chars.to_owned();
        let numbers = match String::from_utf8(number_chars) {
            Ok(v) => v,
            Err(_) => return None,
        };

        return match numbers.parse::<usize>() {
            Ok(v) => Some(v),
            Err(_) => None,
        };
    }
    fn parse_do_dont(&mut self) -> Option<bool> {
        // parse "do"
        self.parse_text("do")?;

        let enabled = match self.current()? {
            CHAR_OPEN_BRACKET => {
                // Parse remainder of "do()"
                self.next()?;
                true
            }
            CHAR_N => {
                // Parse remainder of "don't"
                self.next()?;
                self.parse_text("'t(")?;
                false
            }
            _ => return None,
        };

        if self.next()? != CHAR_CLOSE_BRACKET {
            return None;
        }

        self.next()?;

        Some(enabled)
    }
    fn parse_text(&mut self, text: &str) -> Option<()> {
        for c in text.as_bytes() {
            if self.current()? != *c {
                return None;
            }
            self.next();
        }
        Some(())
    }
}
