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

use expnum::ExpNum;



