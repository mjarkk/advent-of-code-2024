use std::fs;

fn main() {
    let content = fs::read("./puzzle.txt").unwrap();

    let mut even_number = false;
    let mut set_a = [0u32; 1024];
    let mut set_a_offset = 0usize;
    let mut set_b = [0u32; 1024];
    let mut set_b_totals = [0u32; 1024];

    let mut number = 0u32;
    let mut number_index = 0;
    let zero = '0' as u8;
    let nine = '9' as u8;
    for mut c in content {
        if c < zero || c > nine {
            continue;
        }
        c = c - zero;

        let exp = 10u32.pow(4 - number_index);
        number += (c as u32) * exp;
        number_index += 1;
        if number_index < 5 {
            continue;
        }

        if even_number {
            for (idx, value) in set_b.iter().enumerate() {
                if *value == 0 {
                    // We reached the end, append a new number
                    set_b[idx] = number;
                    set_b_totals[idx] = number;
                    break;
                } else if *value == number {
                    // We found a match, add the number to the total
                    set_b_totals[idx] += number;
                    break;
                }
            }
        } else {
            set_a[set_a_offset] = number;
            set_a_offset += 1;
        }

        even_number = !even_number;
        number_index = 0;
        number = 0;
    }

    let mut total = 0u32;
    for number_under_inspect in set_a {
        for (idx, number) in set_b.iter().enumerate() {
            if *number == 0 {
                break;
            }

            if *number == number_under_inspect {
                total += set_b_totals[idx];
                break;
            }
        }
    }

    println!("{}", total);
}
