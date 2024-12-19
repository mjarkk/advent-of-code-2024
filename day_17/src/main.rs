mod program;

use program::{initial_program, run, INITIAL_A};
use std::time::Instant;

fn main() {
    let now = Instant::now();

    let mut output = [0u8; 16];
    let written = run(INITIAL_A, &mut output);

    print!("p1: ");
    print_output(&output, written);

    let program = initial_program();
    let result = get_register_a_from_output(0, &program, &mut output, program.len() - 1).unwrap();

    println!("p2: {}", result);

    println!("Elapsed: {:.2?}", now.elapsed());
}

fn print_output(output: &[u8], read: usize) {
    for (idx, entry) in output.iter().enumerate() {
        if idx == 0 {
        } else if idx >= read {
            break;
        } else {
            print!(",")
        }
        print!("{}", entry);
    }
    println!();
}

fn get_register_a_from_output(
    base: usize,
    expected: &[u8],
    program_output: &mut [u8],
    idx: usize,
) -> Option<usize> {
    let start_offset = if base == 0 { 1 } else { 0 };

    for a in start_offset..8 {
        let new_nr = base + (a << (idx * 3));

        run(new_nr, program_output);

        if program_output[idx] == expected[idx] {
            if idx == 0 {
                return Some(new_nr);
            }

            if let Some(v) = get_register_a_from_output(new_nr, expected, program_output, idx - 1) {
                return Some(v);
            }
        }
    }

    None
}
