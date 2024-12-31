use std::fs;
use std::time::Instant;

fn main() {
    let now = Instant::now();

    let contents = fs::read_to_string("./puzzle.txt").unwrap();
    let mut safe_reports = 0;

    let mut reports: Vec<Vec<i16>> = Vec::new();
    for line in contents.lines() {
        let mut line_numbers: Vec<i16> = Vec::new();
        for word in line.split(' ') {
            let number = word.parse::<i16>().unwrap();
            line_numbers.push(number);
        }
        reports.push(line_numbers);
    }

    for report in reports {
        // Check if the full collection is valid
        if collection_valid(&report) {
            safe_reports += 1;
            continue;
        }

        // Check if a report with a single entry missing is valid
        for idx in 0..report.len() {
            let mut new_report = report.clone();
            new_report.remove(idx);
            if collection_valid(&new_report) {
                safe_reports += 1;
                break;
            }
        }
    }

    println!("{}", safe_reports);
    println!("Elapsed: {:.2?}", now.elapsed());
}

fn collection_valid(line: &Vec<i16>) -> bool {
    let mut direction_up: Option<bool> = None;
    let mut needle = 0;
    for (idx, number) in line.iter().enumerate() {
        if idx == 0 {
            needle = *number;
            continue;
        }

        let raw_diff = number - needle;
        let diff = i16::abs(raw_diff);
        if diff == 0 || diff > 3 {
            return false;
        }

        let new_direction_up = raw_diff > 0;
        if let Some(direction_up_value) = direction_up {
            if direction_up_value != new_direction_up {
                return false;
            }
        } else {
            direction_up = Some(new_direction_up);
        }

        needle = *number;
    }

    return true;
}
