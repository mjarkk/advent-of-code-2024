// Auto generated by build.rs, do not modify

pub fn initial_program() -> [u8; 16] {
    [2, 4, 1, 1, 7, 5, 4, 6, 0, 3, 1, 4, 5, 5, 3, 0]
}

pub const INITIAL_A: usize = 28066687;
pub const INITIAL_B: usize = 0;
pub const INITIAL_C: usize = 0;

pub fn run(initial_a: usize, out: &mut [u8]) -> usize {
    let mut register_a = initial_a;
    #[allow(unused_assignments)]
    let mut register_b = INITIAL_B;
    #[allow(unused_assignments)]
    let mut register_c = INITIAL_C;
    let mut out_idx = 0;

    loop {
        register_b = register_a % 8;
        register_b ^= 1;
        register_c = register_a / 2usize.pow(register_b as u32);
        register_b ^= register_c;
        register_a /= 2usize.pow(3u32);
        register_b ^= 4;
        out[out_idx] = (register_b % 8) as u8;
        out_idx += 1;
        if register_a == 0 {
            break;
        }
    }

    out_idx
}