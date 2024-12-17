use std::fs;
use std::time::Instant;

const ADV: u8 = 0;
const BXL: u8 = 1;
const BST: u8 = 2;
const JNZ: u8 = 3;
const BXC: u8 = 4;
const OUT: u8 = 5;
const BDV: u8 = 6;
const CDV: u8 = 7;

struct Vm {
    register_a: usize,
    register_b: usize,
    register_c: usize,
    initial_b: usize,
    initial_c: usize,
    program: Vec<u8>,
    instruction_pointer: usize,
    out: Vec<u8>,
}

fn main() {
    let now = Instant::now();
    let puzzle = fs::read_to_string("./puzzle.txt").unwrap();

    let mut vm = Vm {
        register_a: 0,
        register_b: 0,
        register_c: 0,
        initial_b: 0,
        initial_c: 0,
        program: Vec::new(),
        instruction_pointer: 0,
        out: Vec::new(),
    };

    for (idx, line) in puzzle.lines().enumerate() {
        match idx {
            0 => {
                vm.register_a = line.split(' ').last().unwrap().parse().unwrap();
            }
            1 => {
                vm.initial_b = line.split(' ').last().unwrap().parse().unwrap();
                vm.register_b = vm.initial_b;
            }
            2 => {
                vm.initial_c = line.split(' ').last().unwrap().parse().unwrap();
                vm.register_c = vm.initial_c;
            }
            4 => {
                for nr_str in line.split_once("Program: ").unwrap().1.split(',') {
                    vm.program.push(nr_str.trim().parse().unwrap());
                }
            }
            _ => {}
        }
    }

    vm.execute(false);

    print!("p1: ");
    vm.print_out();

    let result = vm
        .trace_back_register_a_from_output(0, &vm.program.clone(), vm.program.len() - 1)
        .unwrap();
    println!("p2: {}", result);
    println!("Elapsed: {:.2?}", now.elapsed());
}

impl Vm {
    fn combo_operand(&self) -> usize {
        let combo_operand_selector = self.program[self.instruction_pointer + 1];
        match combo_operand_selector {
            0 | 1 | 2 | 3 => combo_operand_selector as usize,
            4 => self.register_a,
            5 => self.register_b,
            6 => self.register_c,
            _ => panic!("Unknown combo operand selector {}", combo_operand_selector),
        }
    }

    fn reset(&mut self, register_a: usize) {
        self.register_a = register_a;
        self.register_b = self.initial_b;
        self.register_c = self.initial_c;
        self.instruction_pointer = 0;
        self.out.clear();
    }

    fn execute(&mut self, debug: bool) {
        loop {
            let mut jump = true;
            let instruction = self.program[self.instruction_pointer];

            match instruction {
                ADV => {
                    // The adv instruction (opcode 0) performs division. The numerator is the value in the A register.
                    // The denominator is found by raising 2 to the power of the instruction's combo operand.
                    // (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.)
                    // The result of the division operation is truncated to an integer and then written to the A register.

                    let combo_operand = self.combo_operand();
                    let result = self.register_a / 2usize.pow(combo_operand as u32);
                    if debug {
                        println!(
                            "ADV A:{} / (2 ** OP:{}) = A:{}",
                            self.register_a, combo_operand, result
                        );
                    }
                    self.register_a = result;
                }
                BXL => {
                    // The bxl instruction (opcode 1) calculates the bitwise XOR of register B and
                    // the instruction's literal operand, then stores the result in register B.

                    let literal_operand = self.program[self.instruction_pointer + 1];
                    let result = self.register_b ^ literal_operand as usize;
                    if debug {
                        println!(
                            "BXL B:{} ^ OP:{} = B:{}",
                            self.register_b, literal_operand, result
                        );
                    }
                    self.register_b = result;
                }
                BST => {
                    // The bst instruction (opcode 2) calculates the value of its combo operand modulo 8
                    // (thereby keeping only its lowest 3 bits), then writes that value to the B register.

                    let combo_operand = self.combo_operand();
                    self.register_b = combo_operand % 8;
                    if debug {
                        println!("BST OP:{} % 8 = B:{}", combo_operand, self.register_b);
                    }
                }
                JNZ => {
                    // The jnz instruction (opcode 3) does nothing if the A register is 0.
                    // However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand;
                    // if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.

                    let literal_operand = self.program[self.instruction_pointer + 1];
                    if self.register_a != 0 {
                        self.instruction_pointer = literal_operand as usize;
                        jump = false;
                    }
                }
                BXC => {
                    // The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C,
                    // then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)

                    let result = self.register_b ^ self.register_c;
                    if debug {
                        println!(
                            "BXC B:{} ^ C:{} = B:{}",
                            self.register_b, self.register_c, result
                        );
                    }
                    self.register_b = result;
                }
                OUT => {
                    // The out instruction (opcode 5) calculates the value of its combo operand modulo 8,
                    // then outputs that value. (If a program outputs multiple values, they are separated by commas.)

                    let combo_operand = self.combo_operand();
                    let out = combo_operand % 8;
                    self.out.push(out as u8);
                    if debug {
                        println!("OUT OP:{} % 8 = {}", combo_operand, out);
                        println!(
                            "A: {} B: {} C: {}",
                            self.register_a, self.register_b, self.register_c
                        );
                        println!();
                    }
                }
                BDV => {
                    // The bdv instruction (opcode 6) works exactly like the adv instruction except
                    // that the result is stored in the B register. (The numerator is still read from the A register.)

                    let combo_operand = self.combo_operand();
                    self.register_b = self.register_a / 2usize.pow(combo_operand as u32);
                    if debug {
                        println!(
                            "BDV A:{} / (2 ** OP:{}) = B:{}",
                            self.register_a, combo_operand, self.register_b
                        );
                    }
                }
                CDV => {
                    // The cdv instruction (opcode 7) works exactly like the adv instruction except
                    // that the result is stored in the C register. (The numerator is still read from the A register.)

                    let combo_operand = self.combo_operand();
                    self.register_c = self.register_a / 2usize.pow(combo_operand as u32);
                    if debug {
                        println!(
                            "CDV A:{} / (2 ** OP:{}) = C:{}",
                            self.register_a, combo_operand, self.register_c
                        );
                    }
                }
                v => panic!("Unknown instruction opcode {}", v),
            };

            if jump {
                self.instruction_pointer += 2;
            }
            if self.instruction_pointer >= self.program.len() {
                break;
            }
        }
    }

    fn print_out(&self) {
        for (idx, entry) in self.out.iter().enumerate() {
            if idx != 0 {
                print!(",")
            }
            print!("{}", entry);
        }
        println!();
    }

    fn trace_back_register_a_from_output(
        &mut self,
        base: usize,
        output: &Vec<u8>,
        idx: usize,
    ) -> Option<usize> {
        let start_offset = if base == 0 { 1 } else { 0 };

        for a in start_offset..8 {
            let new_nr = base + (a << (idx * 3));
            self.reset(new_nr);
            self.execute(false);
            if self.out[idx] == output[idx] {
                if idx == 0 {
                    return Some(new_nr);
                }

                if let Some(v) = self.trace_back_register_a_from_output(new_nr, output, idx - 1) {
                    return Some(v);
                }
            }
        }

        None
    }
}
