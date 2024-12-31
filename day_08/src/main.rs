use std::collections::{HashMap, HashSet};
use std::fs;
use std::time::Instant;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct Cord {
    x: i16,
    y: i16,
}

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut nodes_all_freq: HashMap<char, HashSet<Cord>> = HashMap::new();
    let mut map_size = Cord { x: 0i16, y: 0i16 };

    for (y, line) in puzzle.lines().enumerate() {
        if line.is_empty() {
            continue;
        }

        map_size.y += 1;
        if y == 0 {
            map_size.x = line.len() as i16;
        }

        for (x, c) in line.chars().enumerate() {
            if c == '.' {
                continue;
            }

            nodes_all_freq
                .entry(c)
                .or_insert(HashSet::new())
                .insert(Cord {
                    x: x as i16,
                    y: y as i16,
                });
        }
    }

    let mut antinodes_a: HashSet<Cord> = HashSet::new();
    let mut antinodes_b: HashSet<Cord> = HashSet::new();
    for (_, nodes) in nodes_all_freq.iter() {
        for (node_a_idx, node_a) in nodes.iter().enumerate() {
            for (node_b_idx, node_b) in nodes.iter().enumerate() {
                if node_a_idx == node_b_idx {
                    continue;
                }

                let diff_x = node_b.x - node_a.x;
                let diff_y = node_b.y - node_a.y;

                let new_a_antinode = Cord {
                    x: node_b.x + diff_x,
                    y: node_b.y + diff_y,
                };
                if !outside_map(&map_size, &new_a_antinode) {
                    antinodes_a.insert(new_a_antinode);
                }

                let mut new_antinode = node_b.clone();
                antinodes_b.insert(new_antinode.clone());
                loop {
                    new_antinode.x += diff_x;
                    new_antinode.y += diff_y;
                    if outside_map(&map_size, &new_antinode) {
                        break;
                    }

                    antinodes_b.insert(new_antinode.clone());
                }
            }
        }
    }

    println!("p1: {}", antinodes_a.len());
    println!("p2: {}", antinodes_b.len());

    println!("Elapsed: {:.2?}", now.elapsed());
}

fn outside_map(map_size: &Cord, node: &Cord) -> bool {
    node.x < 0 || node.y < 0 || node.x >= map_size.x || node.y >= map_size.y
}
