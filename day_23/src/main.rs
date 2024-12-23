use std::collections::{HashMap, HashSet};
use std::time::Instant;
use std::{fs, vec};

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut next_node_id = 0u16;
    let mut computer_name_lookup: HashMap<u16, String> = HashMap::new();
    let mut computer_id_lookup: HashMap<String, u16> = HashMap::new();
    let mut network_map: HashMap<u16, Vec<u16>> = HashMap::new();
    // Instaid of looking up the computer name from the id we can just add a binary flag to
    // the id to indicate if the computer name starts with a 't'.
    // This is required to solve part 1.
    let ends_with_t_flag = 1u16 << 15;
    for line in puzzle.lines() {
        if line.is_empty() {
            continue;
        }

        let (left_computer, right_computer) = line.split_once("-").unwrap();
        let left_id = match computer_id_lookup.get(left_computer) {
            Some(id) => *id,
            None => {
                next_node_id += 1;
                let mut node_id = next_node_id;
                if left_computer.starts_with('t') {
                    node_id |= ends_with_t_flag;
                }

                computer_id_lookup.insert(left_computer.to_string(), node_id);
                computer_name_lookup.insert(node_id, left_computer.to_string());
                node_id
            }
        };
        let right_id = match computer_id_lookup.get(right_computer) {
            Some(id) => *id,
            None => {
                next_node_id += 1;
                let mut node_id = next_node_id;
                if right_computer.starts_with('t') {
                    node_id |= ends_with_t_flag;
                }

                computer_id_lookup.insert(right_computer.to_string(), node_id);
                computer_name_lookup.insert(node_id, right_computer.to_string());
                node_id
            }
        };

        let combinations = [(left_id, right_id), (right_id, left_id)];
        for combination in combinations.iter() {
            let (a, b) = combination;

            let left_set = network_map.entry(*a).or_insert(Vec::new());
            left_set.push(*b);
        }
    }

    // Find solution for part 1
    let mut result = 0;
    let mut found_links: HashSet<(u16, u16, u16)> = HashSet::new();
    for (computer_id, connected_computers) in network_map.iter() {
        for connected_computer_id in connected_computers {
            let links = search_network_of_3(&network_map, *computer_id, *connected_computer_id, 1);
            for mut link in links {
                link.sort();

                let link_key = (link[0], link[1], link[2]);
                if !found_links.insert(link_key) {
                    continue;
                }

                for id in link {
                    if id & ends_with_t_flag == ends_with_t_flag {
                        result += 1;
                        break;
                    }
                }
            }
        }
    }
    println!("{}", result);

    // Find solution for part 2
    let search_size = 8;

    for (base_id, connections) in network_map.iter() {
        if connections.len() < search_size {
            continue;
        }

        for connection_id in connections {
            let connection = match network_map.get(&connection_id) {
                Some(entry) => entry,
                None => continue,
            };

            if connection.len() < search_size {
                continue;
            }
        }

        println!("potential: {}", base_id);
    }

    println!("Elapsed: {:.2?}", now.elapsed());
}

fn search_network_of_3(
    network_map: &HashMap<u16, Vec<u16>>,
    search_for_computer: u16,
    current_computer: u16,
    n: u8,
) -> Vec<Vec<u16>> {
    let connected_computers = match network_map.get(&current_computer) {
        Some(computers) => computers,
        None => return Vec::new(),
    };

    if n == 2 {
        for computer in connected_computers {
            if *computer == search_for_computer {
                return vec![vec![search_for_computer, current_computer]];
            }
        }

        return Vec::new();
    }

    let mut response = Vec::new();
    for computer in connected_computers {
        if *computer != search_for_computer {
            let mut result =
                search_network_of_3(network_map, search_for_computer, *computer, n + 1);
            response.append(&mut result);
        }
    }

    for path in response.iter_mut() {
        path.push(current_computer.clone());
    }
    return response;
}
