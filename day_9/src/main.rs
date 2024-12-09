use std::fs;
use std::time::Instant;

#[derive(Clone)]
struct Block {
    size: usize,
    file_id: Option<usize>,
}

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut raw_output: Vec<Option<usize>> = Vec::new();
    let mut blocks: Vec<Block> = Vec::new();
    let mut empty_indexes: Vec<usize> = Vec::new();
    let mut file_id = 0;
    let mut is_file_blok = true;
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
            blocks.push(Block {
                size: value,
                file_id: Some(file_id),
            });
            file_id += 1;
        } else {
            for _ in 0..value {
                raw_output.push(None);
                empty_indexes.push(raw_output.len() - 1);
            }
            blocks.push(Block {
                size: value,
                file_id: None,
            });
        }
        is_file_blok = !is_file_blok;
    }

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

    for idx in (0..blocks.len()).rev() {
        let block = blocks.get(idx).unwrap();
        if idx == 1 {
            break;
        }
        if block.file_id.is_none() {
            continue;
        }

        // Find the first empty block that can fit this block
        let mut empty_block_idx = None;
        for needle_idx in 0..idx {
            if needle_idx >= idx {
                break;
            }

            let needle = blocks.get(needle_idx).unwrap();
            if needle.file_id.is_none() && needle.size >= block.size {
                empty_block_idx = Some(needle_idx);
                break;
            }
        }

        let (empty_block, empty_block_idx) = match empty_block_idx {
            None => continue,
            Some(empty_block_idx) => (blocks.get(empty_block_idx).unwrap(), empty_block_idx),
        };

        let mut new_blocks = blocks.clone();

        new_blocks[idx] = Block {
            size: block.size,
            file_id: None,
        };

        if empty_block.size == block.size {
            // Replace this block with ours
            new_blocks[empty_block_idx] = block.clone();
        } else {
            // Split the block
            let mut new_empty_block = empty_block.clone();
            new_empty_block.size -= block.size;
            new_blocks[empty_block_idx] = block.clone();
            new_blocks.insert(empty_block_idx + 1, new_empty_block);
        }

        blocks = new_blocks;
    }

    let mut check_sum = 0;
    let mut offset = 0;
    for block in blocks.iter() {
        if let Some(file_id) = block.file_id {
            for idx in 0..block.size {
                check_sum += (idx + offset) * file_id;
            }
        }
        offset += block.size;
    }

    println!("p2: {}", check_sum);
    println!("Elapsed: {:.2?}", now.elapsed());
}
