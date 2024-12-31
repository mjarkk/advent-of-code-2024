use std::time::Instant;
use std::{fs, usize};

#[derive(Clone)]
struct File {
    size: usize,
    id: usize,
}

#[derive(Clone)]
struct Empty {
    size: usize,
    position: usize,
}

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut raw_output: Vec<Option<usize>> = Vec::new();
    let mut empty_indexes: Vec<usize> = Vec::new();
    let mut file_positions: Vec<usize> = Vec::new();
    let mut files: Vec<File> = Vec::new();
    let mut emptyness: Vec<Empty> = Vec::new();
    let mut file_id = 0;
    let mut is_file_blok = true;
    let mut offset = 0;
    for c in puzzle.chars() {
        if c == '\n' {
            continue;
        }

        if c < '0' || c > '9' {
            panic!("Invalid character in file");
        }

        let value = c.to_digit(10).unwrap() as usize;
        if is_file_blok {
            for _ in 0..value {
                raw_output.push(Some(file_id));
            }
            files.push(File {
                size: value,
                id: file_id,
            });
            file_positions.push(offset);
            file_id += 1;
        } else {
            for _ in 0..value {
                raw_output.push(None);
                empty_indexes.push(raw_output.len() - 1);
            }
            emptyness.push(Empty {
                size: value,
                position: offset,
            });
        }
        offset += value;
        is_file_blok = !is_file_blok;
    }

    // let mut data: Vec<char> = (0..offset).map(|_| '_').collect();
    // for (idx, file) in files.iter().enumerate() {
    //     let position = file_positions[idx];
    //     for offset in 0..file.size {
    //         data[position + offset] = file.id.to_string().chars().next().unwrap();
    //     }
    // }
    // for empty in emptyness.iter() {
    //     for offset in 0..empty.size {
    //         data[empty.position + offset] = '.';
    //     }
    // }
    // let debug_data: String = data.iter().collect();
    // println!("{}", debug_data);

    // Solve part 1
    'outer: for empty_index in empty_indexes.iter() {
        if *empty_index >= raw_output.len() {
            break;
        }

        loop {
            match raw_output.pop() {
                Some(Some(v)) => {
                    raw_output[*empty_index] = Some(v);
                    break;
                }
                Some(None) => continue,
                None => break 'outer,
            };
        }
    }

    let mut check_sum = 0;
    for (idx, c) in raw_output.iter().enumerate() {
        if let Some(c) = c {
            check_sum += idx * c;
        }
    }
    println!("p1: {}", check_sum);
    println!("Elapsed: {:.2?}", now.elapsed());

    // Solve part 2
    let mut not_found_solusions_for_size = usize::MAX;
    for (idx, file) in files.iter().enumerate().rev() {
        if file.size >= not_found_solusions_for_size {
            continue;
        }

        let file_position = file_positions[idx];

        // Check if we can place this file in the empty space
        let mut found_empty_segment: Option<(usize, Empty)> = None;
        for (idx, empty_segment) in emptyness.iter().enumerate() {
            if empty_segment.position >= file_position {
                break;
            }

            if file.size <= empty_segment.size {
                found_empty_segment = Some((idx, empty_segment.clone()));
                break;
            }
        }

        let (empty_segment_idx, empty_segment) = match found_empty_segment {
            Some(v) => v,
            None => {
                if file.size < not_found_solusions_for_size {
                    not_found_solusions_for_size = file.size;
                }
                continue;
            }
        };

        // Move the file to the empty segment
        file_positions[idx] = empty_segment.position;

        if empty_segment.size == file.size {
            // Remove the empty segment as file is exactly the same size
            emptyness.remove(empty_segment_idx);
            continue;
        }

        // Update the empty segment as the file is smaller than the empty segment
        emptyness[empty_segment_idx] = Empty {
            size: empty_segment.size - file.size,
            position: empty_segment.position + file.size,
        };
    }

    // let mut data: Vec<char> = (0..offset).map(|_| '_').collect();
    // for (idx, file) in files.iter().enumerate() {
    //     let position = file_positions[idx];
    //     for offset in 0..file.size {
    //         data[position + offset] = file.id.to_string().chars().next().unwrap();
    //     }
    // }
    // for empty in emptyness.iter() {
    //     for offset in 0..empty.size {
    //         data[empty.position + offset] = '.';
    //     }
    // }
    // let debug_data: String = data.iter().collect();
    // println!("{}", debug_data);

    let mut answer_p2 = 0;
    for (idx, file) in files.iter().enumerate() {
        let file_position = file_positions[idx];
        for offset in 0..file.size {
            answer_p2 += file.id * (file_position + offset);
        }
    }

    println!("p2: {}", answer_p2);
    println!("Elapsed: {:.2?}", now.elapsed());
}
