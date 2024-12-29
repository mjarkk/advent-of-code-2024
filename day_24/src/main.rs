use std::collections::{HashMap, HashSet};
use std::io::{self};
use std::{fs, vec};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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
            Operator::AND => a & b,
            Operator::OR => a | b,
            Operator::XOR => a ^ b,
        }
    }

    fn to_str(&self) -> &'static str {
        match self {
            Operator::AND => "&",
            Operator::OR => "|",
            Operator::XOR => "^",
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

impl Gate {
    fn print(&self) {
        println!(
            "{} {} {} = {}",
            self.lefthand,
            self.operator.to_str(),
            self.righthand,
            self.result,
        );
    }
}

fn main() {
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
        result = (result << 1) | value;
    }
    println!("{}", result);

    interactive_solve(gates);
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

fn interactive_solve(mut gates: Vec<Gate>) {
    let mut pre_swap = vec!["z33,fcd", "tpc,rvf", "z20,fhp", "hmk,z16"];
    let mut swaps: Vec<String> = Vec::new();
    loop {
        println!("===============================");
        let mut new_instructions: Vec<Gate> = Vec::new();
        for idx in (0..gates.len()).rev() {
            let gate = &gates[idx];
            if gate.result.starts_with('z') {
                new_instructions.push(gate.clone());
                gates.remove(idx);
            }
        }

        new_instructions.sort_by(|a, b| a.result.cmp(&b.result));

        let mut bogus_output_gates = Vec::new();
        for idx in (0..new_instructions.len() - 1).rev() {
            let gate = &new_instructions[idx];
            if gate.operator != Operator::XOR {
                bogus_output_gates.push(gate.clone());
                new_instructions.remove(idx);
            }
        }

        let mut no_matches = 0;
        'outer: while let Some(gate) = gates.pop() {
            for (idx, new_gate) in new_instructions.iter().enumerate() {
                if new_gate.lefthand == gate.result || new_gate.righthand == gate.result {
                    new_instructions.insert(idx, gate);
                    no_matches = 0;
                    continue 'outer;
                }
            }

            if no_matches > gates.len() {
                break;
            }
            no_matches += 1;

            gates.insert(0, gate);
        }

        while let Some(gate) = gates.pop() {
            new_instructions.push(gate);
        }
        while let Some(gate) = bogus_output_gates.pop() {
            new_instructions.push(gate);
        }

        let mut instruction_collection = Vec::new();
        for gate_idx in 0..new_instructions.len() {
            let gate = &new_instructions[gate_idx];
            instruction_collection.push(gate.clone());
            if !gate.result.starts_with('z') {
                continue;
            }
            let result_nr = gate.result[1..].parse::<u8>().unwrap();

            let mut valid = true;
            let mut ors = 0;
            let mut xors = 0;
            let mut ands = 0;
            let mut and_number = None;
            let mut xor_number = None;
            for gate in instruction_collection.iter() {
                match gate.operator {
                    Operator::AND => ands += 1,
                    Operator::OR => ors += 1,
                    Operator::XOR => xors += 1,
                };
                let lefthand_nr = gate.lefthand[1..].parse::<u8>();
                if let Ok(nr) = lefthand_nr {
                    match gate.operator {
                        Operator::AND => {
                            if and_number.is_none() {
                                and_number = Some(nr);
                            } else {
                                valid = false;
                            }
                        }
                        Operator::OR => valid = false,
                        Operator::XOR => {
                            if xor_number.is_none() {
                                xor_number = Some(nr);
                            } else {
                                valid = false;
                            }
                        }
                    }
                }

                if !valid {
                    break;
                }
            }
            if valid {
                if ors != 1 || xors != 2 || ands != 2 {
                    valid = false;
                } else if and_number.is_none() || xor_number.is_none() {
                    valid = false;
                } else if and_number.unwrap() + 1 != result_nr {
                    valid = false;
                } else if xor_number.unwrap() != result_nr {
                    valid = false;
                }
            }

            if valid {
                instruction_collection.clear();
                continue;
            }

            for gate in instruction_collection.iter() {
                gate.print();
            }
            println!();
            instruction_collection.clear();
        }

        println!();
        println!("===============================");
        println!("Swaps:");
        swaps.sort();
        for (idx, swap) in swaps.iter().enumerate() {
            if idx > 0 {
                print!(",");
            }
            print!("{}", swap);
        }
        println!();
        println!("Perfect gate:");
        println!("  ntn & dtn = wsm");
        println!("  x28 & y28 = fnj");
        println!("  fnj | wsm = ksv");
        println!("  y29 ^ x29 = scf");
        println!("  scf ^ ksv = z29");
        println!();
        loop {
            let output = match pre_swap.pop() {
                Some(v) => v.to_string(),
                None => {
                    println!("Which output gates do you want to swap (FOO,BAR)?");
                    let mut output = String::new();
                    io::stdin().read_line(&mut output).unwrap();
                    output
                }
            };

            let (left, mut right) = match output.split_once(",") {
                Some(v) => v,
                None => {
                    println!("Invalid input");
                    continue;
                }
            };
            right = right.trim();

            let mut seen_left = false;
            let mut seen_right = false;
            for gate in new_instructions.iter() {
                if gate.result == left {
                    seen_left = true;
                } else if gate.result == right {
                    seen_right = true;
                }
            }
            if !seen_left || !seen_right {
                println!("Invalid input (gates not found)");
                continue;
            }

            for idx in 0..new_instructions.len() {
                let gate = &new_instructions[idx];
                if gate.result == left {
                    new_instructions[idx].result = right.to_string();
                } else if gate.result == right {
                    new_instructions[idx].result = left.to_string();
                }
            }

            swaps.push(left.to_string());
            swaps.push(right.to_string());
            break;
        }

        gates = new_instructions;
    }
}
