use std::collections::{HashMap, HashSet};
use std::io::{self, Read};
use std::time::Instant;
use std::{fs, result, vec};

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

    // let mut bogus_outputs = Vec::new();
    for gate in gates.iter() {
        // if let Operator::AND = gate.operator {
        //     if (gate.lefthand.starts_with('x') || gate.lefthand.starts_with('y'))
        //         && (gate.righthand.starts_with('x') || gate.righthand.starts_with('y'))
        //         && gate.result.starts_with('z')
        //     {
        //         bogus_outputs.push(gate);
        //     }
        // }

        // gate.print();
        // if gate.result.starts_with('z') {
        //     println!();
        // }

        let left_hand = states.get(&gate.lefthand).unwrap();
        let right_hand = states.get(&gate.righthand).unwrap();
        let result = gate.operator.apply(*left_hand, *right_hand);
        states.insert(gate.result.to_string(), result);
    }
    // println!("bogus outputs: {:?}", bogus_outputs);

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

    interactive_solve(gates);
    // solve(gates);

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

fn solve(mut gates: Vec<Gate>) {
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

    'outer: loop {
        let mut last_z_register = String::new();
        let mut last_instruction_set = Vec::new();
        for (gate_idx, gate) in new_instructions.iter().enumerate() {
            last_instruction_set.push(gate.clone());

            if last_instruction_set.len() == 5 {
                let mut valid = true;
                for (idx, gate) in last_instruction_set.iter().enumerate() {
                    match (idx, gate.operator) {
                        (0, Operator::AND) => {}
                        (1, Operator::AND) => {}
                        (2, Operator::OR) => {}
                        (3, Operator::XOR) => {}
                        (4, Operator::XOR) => {}
                        _ => {
                            valid = false;
                            break;
                        }
                    }
                }

                if valid && !gate.result.starts_with('z') {
                    let expected_z_register_nr = last_z_register[1..].parse::<u8>().unwrap() + 1;
                    let expected_z_register = format!("z{:02}", expected_z_register_nr);

                    // In this sequence we expected the last instruction to write to a z register
                    for (sub_gate_idx, sub_gate) in bogus_output_gates.iter().enumerate() {
                        if sub_gate.result == expected_z_register {
                            let mut new_gate = gate.clone();
                            new_gate.result = sub_gate.result.clone();
                            let mut new_sub_gate = sub_gate.clone();
                            new_sub_gate.result = gate.result.clone();

                            bogus_output_gates.remove(sub_gate_idx);

                            new_instructions[gate_idx] = new_gate;
                            gates.push(new_sub_gate);

                            continue 'outer;
                        }
                    }
                }
            }

            if gate.result.starts_with('z') {
                last_z_register = gate.result.clone();
                last_instruction_set.clear();
            }
        }

        no_matches = 0;
        'inner: while let Some(gate) = gates.pop() {
            for (idx, new_gate) in new_instructions.iter().enumerate() {
                if new_gate.lefthand == gate.result || new_gate.righthand == gate.result {
                    println!("INSERTING");
                    gate.print();
                    new_instructions.insert(idx, gate);
                    no_matches = 0;
                    continue 'inner;
                }
            }

            if no_matches > gates.len() {
                continue 'outer;
            }
            no_matches += 1;

            gates.insert(0, gate);
        }

        break;
    }

    assert_eq!(gates.len(), 0);
    assert_eq!(bogus_output_gates.len(), 0);

    let mut last_instructions = Vec::new();
    for gate_idx in 0..new_instructions.len() {
        let gate = &new_instructions[gate_idx];
        last_instructions.push(gate.clone());
        if !gate.result.starts_with('z') {
            continue;
        }

        let mut valid = true;
        for (idx, gate) in last_instructions.iter().enumerate() {
            match (idx, gate.operator) {
                (0, Operator::AND) => {}
                (1, Operator::AND) => {}
                (2, Operator::OR) => {}
                (3, Operator::XOR) => {}
                (3, Operator::AND) => {
                    // migth know how to fix this
                    let last_instruction = last_instructions.last().unwrap();
                    if gate.lefthand[1..] != last_instruction.result[1..]
                        || gate.righthand[1..] != last_instruction.result[1..]
                    {
                        valid = false;
                        break;
                    }

                    // Lets search for a gate that we can swap this with
                    let mut swap_with_idx = None;
                    for (sub_gate_idx, sub_gate) in new_instructions.iter().enumerate() {
                        if sub_gate.operator == Operator::XOR
                            && sub_gate.lefthand[1..] == last_instruction.result[1..]
                            && sub_gate.righthand[1..] == last_instruction.result[1..]
                        {
                            swap_with_idx = Some(sub_gate_idx);
                            break;
                        }
                    }

                    let swap_with_idx = match swap_with_idx {
                        Some(idx) => idx,
                        None => {
                            valid = false;
                            break;
                        }
                    };

                    let mut new_and = new_instructions[swap_with_idx].clone();
                    let mut new_xor = new_instructions[gate_idx - 1].clone();
                    (new_xor.result, new_and.result) =
                        (new_and.result.clone(), new_xor.result.clone());

                    new_instructions[swap_with_idx] = new_xor;
                    new_instructions[gate_idx - 1] = new_and;
                }
                (4, Operator::XOR) => {}
                _ => {
                    valid = false;
                    break;
                }
            }
        }

        if valid {
            last_instructions.clear();
            continue;
        }

        for offset in 0..last_instructions.len() {
            let gate = &new_instructions[gate_idx - (last_instructions.len() - 1 - offset)];
            gate.print();
        }
        println!();
        last_instructions.clear();
    }

    println!("{} {}", new_instructions.len(), gates.len());
}
