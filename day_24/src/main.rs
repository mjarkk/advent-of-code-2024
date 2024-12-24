use std::collections::{HashMap, HashSet};
use std::fs;
use std::time::Instant;

#[derive(Debug, Clone)]
enum Operator {
    AND,
    OR,
    XOR,
}

impl Operator {
    fn from_str(s: &str) -> Operator {
        match s {
            "AND" => Operator::AND,
            "OR" => Operator::OR,
            "XOR" => Operator::XOR,
            _ => panic!("Invalid operator"),
        }
    }

    fn apply(&self, a: bool, b: bool) -> bool {
        match self {
            Operator::AND => a && b,
            Operator::OR => a || b,
            Operator::XOR => a ^ b,
        }
    }
}

#[derive(Debug, Clone)]
struct Gate {
    lefthand: String,
    operator: Operator,
    righthand: String,
    result: String,
}

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut states: HashMap<String, bool> = HashMap::new();
    let mut gates: Vec<Gate> = Vec::new();
    let mut reading_puzzle_part = 0;
    for line in puzzle.lines() {
        if line == "" {
            reading_puzzle_part += 1;
            continue;
        }

        match reading_puzzle_part {
            0 => {
                let (name, value) = line.split_once(": ").unwrap();
                states.insert(name.to_string(), value == "1");
            }
            1 => {
                let parts: Vec<&str> = line.split(" ").collect();
                gates.push(Gate {
                    lefthand: parts[0].to_string(),
                    operator: Operator::from_str(parts[1]),
                    righthand: parts[2].to_string(),
                    result: parts[4].to_string(),
                });
            }
            _ => { /* Ignore */ }
        }
    }

    // Sort the gates so we can execute them in order
    sort_gates(&mut gates, &mut states.keys().cloned().collect());

    for gate in gates.iter() {
        let left_hand = states.get(&gate.lefthand).unwrap();
        let right_hand = states.get(&gate.righthand).unwrap();
        let result = gate.operator.apply(*left_hand, *right_hand);
        states.insert(gate.result.to_string(), result);
    }

    let mut state_keys: Vec<String> = states.keys().cloned().collect();
    state_keys.sort();
    let mut result = 0u64;
    for key in state_keys.iter().rev() {
        if !key.starts_with("z") {
            continue;
        }

        if key[1..].parse::<u8>().is_err() {
            continue;
        }

        let value = if *states.get(key).unwrap() { 1 } else { 0 };
        // println!("{}: {}", key, value);
        result = (result << 1) | value;
    }

    println!("{}", result);
    println!("Elapsed: {:.2?}", now.elapsed());
}

fn sort_gates(gates: &mut Vec<Gate>, known_variables: &mut HashSet<String>) {
    let mut offset = 0;
    while offset < gates.len() {
        for idx in offset..gates.len() {
            let gate = gates.get(idx).unwrap();

            if known_variables.contains(&gate.righthand) && known_variables.contains(&gate.lefthand)
            {
                known_variables.insert(gate.result.clone());
                let cloned_gate = gate.clone();
                gates.remove(idx);
                gates.insert(offset, cloned_gate);
                offset += 1;
            }
        }
    }
}
