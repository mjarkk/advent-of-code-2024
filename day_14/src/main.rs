use image::{ImageBuffer, Rgb};
use std::collections::HashSet;
use std::fs;
use std::time::Instant;

#[derive(Debug)]
struct Robot {
    position: (isize, isize),
    velocity: (isize, isize),
}

impl Robot {
    fn move_robot(&mut self, map_size: (isize, isize)) {
        self.position.0 += self.velocity.0;
        self.position.1 += self.velocity.1;

        // Warp around the map if the robot goes out of bounds
        if self.position.0 < 0 {
            self.position.0 = map_size.0 + self.position.0;
        } else if self.position.0 >= map_size.0 {
            self.position.0 = self.position.0 - map_size.0;
        }

        if self.position.1 < 0 {
            self.position.1 = map_size.1 + self.position.1;
        } else if self.position.1 >= map_size.1 {
            self.position.1 = self.position.1 - map_size.1;
        }
    }
}

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut robots: Vec<Robot> = Vec::new();
    for line in puzzle.lines() {
        if line.is_empty() {
            continue;
        }

        let mut numbers: Vec<isize> = Vec::with_capacity(4);
        let mut number_chars: Vec<char> = Vec::with_capacity(3);
        for c in line.chars() {
            if c.is_numeric() || c == '-' {
                number_chars.push(c);
                continue;
            }

            if number_chars.is_empty() {
                continue;
            }

            let number: isize = number_chars.iter().collect::<String>().parse().unwrap();
            numbers.push(number);
            number_chars.clear();
        }
        if !number_chars.is_empty() {
            let number: isize = number_chars.iter().collect::<String>().parse().unwrap();
            numbers.push(number);
        }

        assert_eq!(numbers.len(), 4);

        robots.push(Robot {
            position: (numbers[0], numbers[1]),
            velocity: (numbers[2], numbers[3]),
        });
    }
    let map_size = (101, 103);
    // let map_size = (11, 7);

    for i in 0..10_000 {
        let mut robot_positions: HashSet<(isize, isize)> = HashSet::new();
        for robot in robots.iter_mut() {
            robot.move_robot(map_size);
            robot_positions.insert(robot.position);
        }

        let mut found_blok = false;
        'outer: for y in 2..map_size.1 - 2 {
            'inner: for x in 2..map_size.0 - 2 {
                let cord_to_check = [
                    (x, y),
                    (x - 1, y),
                    (x + 1, y),
                    (x, y - 1),
                    (x, y + 1),
                    (x + 1, y + 1),
                    (x + 1, y - 1),
                    (x - 1, y + 1),
                    (x - 1, y - 1),
                ];

                for cord in cord_to_check {
                    if robot_positions.get(&cord).is_none() {
                        continue 'inner;
                    }
                }

                found_blok = true;
                break 'outer;
            }
        }

        if !found_blok {
            continue;
        }

        let size_per_pixel = 5u32;
        let mut image: ImageBuffer<Rgb<u8>, Vec<u8>> = ImageBuffer::new(
            map_size.0 as u32 * size_per_pixel,
            map_size.1 as u32 * size_per_pixel,
        );
        for y in 0..map_size.1 {
            for x in 0..map_size.0 {
                let mut found = false;
                for robot in robots.iter() {
                    if robot.position.0 == x && robot.position.1 == y {
                        found = true;
                        break;
                    }
                }

                if !found {
                    continue;
                }

                // Give the pixel some pretty colors
                let color = [
                    rand::random::<u8>() / 2 + 128,
                    rand::random::<u8>() / 2 + 128,
                    rand::random::<u8>() / 2 + 128,
                ];

                let y_min = y as u32 * size_per_pixel;
                let y_max = (y as u32 * size_per_pixel) + size_per_pixel;
                let x_min = x as u32 * size_per_pixel;
                let x_max = (x as u32 * size_per_pixel) + size_per_pixel;
                for yb in y_min..y_max {
                    for xb in x_min..x_max {
                        image.get_pixel_mut(xb, yb).0 = color;
                    }
                }
            }
        }
        _ = image.save(format!("images/{}.png", i + 1));
    }

    let half_map = (map_size.0 / 2, map_size.1 / 2);
    let mut robots_per_quadrant = [0, 0, 0, 0];
    for robot in robots.iter() {
        let mut quadrant = 0;
        if robot.position.0 <= half_map.0 - 1 {
            // Continue
        } else if robot.position.0 > half_map.0 {
            quadrant += 2;
        } else {
            continue;
        }

        if robot.position.1 <= half_map.1 - 1 {
            // Continue
        } else if robot.position.1 > half_map.1 {
            quadrant += 1;
        } else {
            continue;
        }

        robots_per_quadrant[quadrant] += 1;
    }

    println!("{:?}", robots_per_quadrant);

    let mut result = 1;
    for nr in robots_per_quadrant {
        result *= nr;
    }

    println!("{}", result);
    println!("Elapsed: {:.2?}", now.elapsed());
}
