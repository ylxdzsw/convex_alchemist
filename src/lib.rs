#![allow(clippy::missing_safety_doc)]
#![feature(vec_into_raw_parts)]

// #[no_mangle]
// unsafe extern fn hello(str_pointer_buffer: *mut u32) {
//     if let [ptr, len, capacity] = std::slice::from_raw_parts_mut(str_pointer_buffer, 3) {
//         let obj = serde_json::json!({
//             "hello": "world",
//             "random": get_random_u32(),
//             "array": vec![0.1, 0.2]
//         });

//         let raw_parts = serde_json::to_vec(&obj).unwrap().into_raw_parts();
        
//         *ptr = raw_parts.0 as _;
//         *len = raw_parts.1 as _;
//         *capacity = raw_parts.2 as _;
//     } else {
//         panic!("bug");
//     }
// }

use expnum::ExpNum;
mod expnum {
    #[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
    pub struct ExpNum(f64);

    impl ExpNum {
        pub fn from_exp(f: impl Into<f64>) -> Self {
            let f = f.into();

            if f.is_nan() || (f > 0. && f.is_infinite()) {
                panic!("bad number");
            }

            Self(f)
        }

        pub fn pow<T: Into<f64>>(self, rhs: T) -> Self {
            Self(self.0 * rhs.into())
        }
    }

    impl From<f64> for ExpNum {
        fn from(f: f64) -> Self {
            Self(f.ln())
        }
    }

    impl From<ExpNum> for f64 {
        fn from(e: ExpNum) -> Self {
            e.0.exp()
        }
    }

    impl std::ops::Add for ExpNum {
        type Output = Self;

        fn add(self, rhs: Self) -> Self::Output {
            let (larger, smaller) = if self.0 > rhs.0 {
                (self.0, rhs.0)
            } else {
                (rhs.0, self.0)
            };
            Self(larger + (smaller - larger).exp().ln_1p())
        }
    }

    impl std::ops::Sub for ExpNum {
        type Output = Self;

        fn sub(self, rhs: Self) -> Self::Output {
            assert!(self >= rhs);
            Self(self.0 + (-(rhs.0 - self.0).exp()).ln_1p())
        }
    }

    impl std::ops::Mul for ExpNum {
        type Output = Self;

        fn mul(self, rhs: Self) -> Self::Output {
            Self(self.0 + rhs.0)
        }
    }

    impl std::ops::Div for ExpNum {
        type Output = Self;

        fn div(self, rhs: Self) -> Self::Output {
            Self(self.0 - rhs.0)
        }
    }

    impl std::fmt::Display for ExpNum {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "e^{}", self.0)
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_expnum() {
            let a = ExpNum::from_exp(0);
            let b = ExpNum::from(1.);
            assert!(f64::from(a - b) < 1e-9);

            let a = ExpNum::from_exp(f64::NEG_INFINITY);
            let b = ExpNum::from(1.);
            assert!(f64::from((b + a) - b) < 1e-9);
            
            let a = ExpNum::from_exp(800);
            let b = ExpNum::from(1.);
            assert!(f64::from((b + a) - a) < 1e-9);

            let a = ExpNum::from_exp(800);
            let b = ExpNum::from_exp(800);
            assert!(f64::from((a + b) / b) - 2. < 1e-9);

            let a = ExpNum::from_exp(800);
            let b = ExpNum::from_exp(500);
            let c = ExpNum::from_exp(1300);
            assert!(f64::from(a * b - c) < 1e-9);

            let a = ExpNum::from_exp(800);
            let b = ExpNum::from_exp(1);
            assert!(f64::from((a - b) - a) < 1e-9);

            let a = ExpNum::from_exp(800);
            let b = ExpNum::from_exp(1600);
            assert!(f64::from(a.pow(2) - b) < 1e-9);
        }
    }
}

struct Game {
    rand_state: u32
}

impl Game {
    fn new(rand_state: u32) -> Game {
        Game {
            rand_state
        }
    }

    fn get_random_u32(&mut self) -> u32 {
        self.rand_state ^= self.rand_state << 13;
        self.rand_state ^= self.rand_state >> 17;
        self.rand_state ^= self.rand_state << 5;
        self.rand_state
    }

    fn rand(&mut self) -> f64 {
        (self.get_random_u32() % 1000000) as f64 / 1000000.
    }
}


#[no_mangle]
unsafe extern fn alloc(byte_size: usize) -> *mut u8 {
    let (ptr, _len, _capacity) = Vec::with_capacity(byte_size).into_raw_parts();
    ptr
}

#[no_mangle]
unsafe extern fn free(ptr: *mut u8, byte_size: usize) {
    let _ = Vec::from_raw_parts(ptr, 0, byte_size);
}

#[no_mangle]
unsafe extern fn game_new(rand_seed: u32) -> *mut Game {
    Box::into_raw(Box::new(Game::new(rand_seed)))
}

#[no_mangle]
unsafe extern fn game_free(game: *mut Game) {
    let _ = Box::from_raw(game);
}

