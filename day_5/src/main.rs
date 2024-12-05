use std::collections::HashMap;
use std::fs;

fn main() {
    let contents = fs::read_to_string("./puzzle.txt").unwrap();
    let mut section = 0usize;
    let mut requirements: HashMap<u16, Vec<u16>> = HashMap::new();
    let mut updates: Vec<Vec<u16>> = Vec::new();
    for line in contents.lines() {
        if line == "" {
            section += 1;
            continue;
        }
        match section {
            0 => {
                let parts: Vec<&str> = line.split('|').collect();
                let before: u16 = parts[0].parse().unwrap();
                let after: u16 = parts[1].parse().unwrap();

                match requirements.get_mut(&after) {
                    Some(v) => {
                        v.push(before);
                    }
                    None => {
                        requirements.insert(after, vec![before]);
                    }
                };
            }
            1 => {
                let mut update: Vec<u16> = Vec::new();
                for nr_str in line.split(',') {
                    update.push(nr_str.parse().unwrap());
                }
                updates.push(update);
            }
            _ => break,
        }
    }

    // Part 1 Check for updates that are already in the right order
    let mut p1_solusion = 0usize;
    let mut bogus_updates: Vec<Vec<u16>> = Vec::new();
    'outer: for update in updates.iter() {
        for (idx, num) in update.iter().enumerate() {
            let requirements_for_num = match requirements.get(num) {
                Some(v) => v,
                None => continue,
            };

            // Check if each requirement is not on the right side of the number
            for req in requirements_for_num {
                for j in idx..update.len() {
                    let entry = update[j];
                    if entry == *req {
                        bogus_updates.push(update.clone());
                        continue 'outer;
                    }
                }
            }
        }

        p1_solusion += update[update.len() / 2] as usize;
    }
    println!("{}", p1_solusion);

    // Part 2 Try to fix the updates
    let mut p2_solusion = 0usize;
    for update in bogus_updates {
        let mut new_update = update.clone();
        for i in 0..new_update.len() {
            let mut itr = 0;
            'outer: loop {
                itr += 1;
                if itr > 100 {
                    println!("going in loops");
                    break;
                }

                let num = new_update[i];
                let requirements_for_num = match requirements.get(&num) {
                    Some(v) => v,
                    None => break,
                };

                // try to swap the numbers that are invalid
                for req in requirements_for_num {
                    for j in i..new_update.len() {
                        let entry = new_update[j];
                        if entry == *req {
                            new_update.swap(i, j);
                            continue 'outer;
                        }
                    }
                }

                break;
            }
        }

        p2_solusion += new_update[new_update.len() / 2] as usize;
    }
    println!("{}", p2_solusion);
}
