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
    println!("p1: {}", result);

    // Find solution for part 2
    let min_search_size = 10;
    let mut biggest_batch = Vec::new();
    let mut skips: Vec<bool> = Vec::with_capacity(16);
    let mut hits_connected_node: Vec<(u16, Vec<bool>)> = Vec::with_capacity(16);
    for (base_id, connections) in network_map.iter() {
        // Skip networks that are to small for part 2
        if connections.len() < min_search_size || connections.len() <= biggest_batch.len() {
            continue;
        }

        // Firstly lets just notate information about the connections that are connected to the base node
        // We mainly check if each child node is connected to all other child nodes and note that down.
        // Beside that we check here already for some bogus nodes that we can probably skip.
        hits_connected_node.clear();
        skips.clear();
        for connection_id in connections.iter() {
            let node_connections = match network_map.get(&connection_id) {
                Some(entry) => entry,
                None => continue,
            };

            if node_connections.len() < min_search_size {
                continue;
            }

            let mut valid_conns = Vec::new();
            let mut valid_conns_count = 0;
            for compare_connection_id in connections.iter() {
                let connected = compare_connection_id == connection_id
                    || node_connections.contains(compare_connection_id);
                valid_conns.push(connected);
                if connected {
                    valid_conns_count += 1;
                }
            }
            if valid_conns_count > 3 {
                hits_connected_node.push((*connection_id, valid_conns));
                skips.push(false);
            } else {
                skips.push(true);
            }
        }

        // At this point we collect all the nodes that are connected to every other node
        // We skip the nodes that have missing connections except for the connections that are very loosly connected. (skips)
        let mut ids = Vec::new();
        'outer: for line in hits_connected_node.iter() {
            for (idx, value) in line.1.iter().enumerate() {
                if !skips[idx] && !*value {
                    continue 'outer;
                }
            }

            ids.push(line.0);
        }

        if ids.len() + 1 > biggest_batch.len() {
            ids.push(*base_id);
            biggest_batch = ids;
        }
    }

    let mut biggest_batch_computer_names = Vec::with_capacity(biggest_batch.len());
    for id in biggest_batch {
        biggest_batch_computer_names.push(computer_name_lookup.get(&id).unwrap());
    }
    biggest_batch_computer_names.sort();
    print!("p2: ");
    for (idx, name) in biggest_batch_computer_names.iter().enumerate() {
        if idx != 0 {
            print!(",");
        }
        print!("{}", name);
    }
    println!();

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
