#![allow(clippy::missing_safety_doc)]
#![feature(vec_into_raw_parts)]

static mut RANDOM: u32 = 39393;

pub fn get_random_u32() -> u32 {
    unsafe {
        RANDOM ^= RANDOM << 13;
        RANDOM ^= RANDOM >> 17;
        RANDOM ^= RANDOM << 5;
        RANDOM
    }
}

#[no_mangle]
pub unsafe extern fn set_random_seed(seed: u32) {
    RANDOM = seed
}

#[no_mangle]
pub unsafe extern fn alloc_memory(byte_size: usize) -> *mut u8 {
    let (ptr, _len, _capacity) = Vec::with_capacity(byte_size).into_raw_parts();
    ptr
}

#[no_mangle]
pub unsafe extern fn free_memory(ptr: *mut u8, byte_size: usize) {
    let _ = Vec::from_raw_parts(ptr, 0, byte_size);
}

#[no_mangle]
pub unsafe extern fn hello(str_pointer_buffer: *mut u32) {
    if let [ptr, len, capacity] = std::slice::from_raw_parts_mut(str_pointer_buffer, 3) {
        let obj = serde_json::json!({
            "hello": "world",
            "random": get_random_u32(),
            "array": vec![0.1, 0.2]
        });

        let raw_parts = serde_json::to_vec(&obj).unwrap().into_raw_parts();
        
        *ptr = raw_parts.0 as _;
        *len = raw_parts.1 as _;
        *capacity = raw_parts.2 as _;
    } else {
        panic!("bug");
    }
}

mod game;