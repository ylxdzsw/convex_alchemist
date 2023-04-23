#![allow(clippy::missing_safety_doc)]
#![allow(non_upper_case_globals)]
#![feature(vec_into_raw_parts)]
#![feature(generic_arg_infer)]
#![feature(const_trait_impl)]
#![feature(const_mut_refs)]

use std::{collections::BTreeMap, fmt::Debug, ops::{Index, IndexMut}, rc::Rc, f64::consts::LN_10, borrow::Borrow, str::FromStr};
use serde_json::{json, Value as JsonValue, Value::Null as JsonNull};

#[derive(Debug, Clone, Copy)]
struct ExpNum(f64);

static NUMBER_FORMAT_CUTOFF: [ExpNum; 5] = [ // unfortunately float arithmetic is not const
    ExpNum(-6.907755278982137), // 0.001
    ExpNum(0.),
    ExpNum(9.210340371976184), // 10000
    ExpNum(27. * LN_10),
    ExpNum(10000.)
];

impl ExpNum {
    fn from_exp(f: impl Into<f64>) -> Self {
        let f = f.into();

        if f.is_nan() || (f > 0. && f.is_infinite()) {
            panic!("bad number");
        }

        Self(f)
    }

    fn as_exp(&self) -> f64 {
        self.0
    }

    // Somehow behave strangely when 0 is involved, so special case them
    fn zero() -> Self {
        Self(f64::NEG_INFINITY)
    }

    fn is_zero(&self) -> bool {
        self.0 == f64::NEG_INFINITY
    }

    fn pow(self, rhs: impl Into<f64>) -> Self {
        Self(self.0 * rhs.into())
    }

    fn format(&self, format: &str, output_type: &str) -> String {
        if self.is_zero() {
            return String::from("0");
        }
    
        match format {
            "a" => {
                if self < &ExpNum::from(1.) || self > &ExpNum::from_exp(f64::ln(1000.0) * 27.0 - 1e-9) {
                    panic!("out of range: num={}, format={}", self, format);
                }
    
                if self < &ExpNum::from(1000.) {
                    return format!("{:.2}", f64::from(*self));
                }
    
                let scale = (self.as_exp() / f64::ln(1000.0) + 1e-9).floor();
                let significand_f64 = f64::from(*self / ExpNum::from(1000.).pow(ExpNum::from(scale)));
                let significand_str = if significand_f64 < 10.0 {
                    format!("{:.3}", significand_f64)
                } else if significand_f64 < 100.0 {
                    format!("{:.2}", significand_f64)
                } else {
                    format!("{:.1}", significand_f64)
                };
                format!("{}{a}{a}", significand_str, a = (b'a' - 1 + scale as u8) as char)
            }
            "10" => {
                let scale = (self.as_exp() / LN_10 + 1e-9).floor();
                let significand_f64 = f64::from(*self / ExpNum::from_exp(LN_10).pow(ExpNum::from(scale)));
                match output_type {
                    "tex" => format!("<ca-katex>{:.2}\\times{{}}10^{{{}}}</ca-katex>", significand_f64, scale as i32),
                    "html" => format!("{:.2}×10<sup>{}</sup>", significand_f64, scale as i32),
                    "plain" => format!("{:.2}E{}", significand_f64, scale as i32),
                    _ => panic!(),
                }
            }
            "e" => {
                match output_type {
                    "tex" => format!("<ca-katex>e^{{{:.3}}}</ca-katex>", self.as_exp()),
                    "html" => format!("ℯ<sup>{:.3}</sup>", self.as_exp()),
                    "plain" => format!("e{:.3}", self.as_exp()),
                    _ => panic!(),
                }
            }
            "d" => {
                if self >= &ExpNum::from_exp(LN_10 * 10.0) || self < &ExpNum::from(0.001) {
                    panic!("out of range: num={}, format={}", self, format);
                }
                if self > &ExpNum::from(10. - 1e-9) {
                    format!("{:.2}", f64::from(*self))
                } else if self >= &ExpNum::from(1. - 1e-9) {
                    format!("{:.3}", f64::from(*self))
                } else {
                    format!("{:.4}", f64::from(*self))
                }
            }
            "ee" => {
                if self < &ExpNum::from_exp(1e-9) {
                    panic!("out of range: num={}, format={}", self, format);
                }
    
                match output_type {
                    "tex" => format!("<ca-katex>e^{{e^{{{:.3}}}}}</ca-katex>", self.as_exp()),
                    "html" => format!("ℯ<sup>ℯ<sup>{:.3}</sup></sup>", self.as_exp()),
                    "plain" => format!("ee{:.3}", self.as_exp()),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    fn format_with_preference(&self, preference: &[impl Borrow<str>], output_type: &str) -> String {
        let mut i = 0;
        while i < NUMBER_FORMAT_CUTOFF.len() && self > &NUMBER_FORMAT_CUTOFF[i] {
            i += 1;
        }
        return self.format(preference[i].borrow(), output_type);
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
        if self.is_zero() {
            return rhs;
        }

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
        if rhs.is_zero() {
            return self;
        }

        assert!(self >= rhs);
        Self(self.0 + (-(rhs.0 - self.0).exp()).ln_1p())
    }
}

impl std::ops::Mul for ExpNum {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        if self.is_zero() || rhs.is_zero() {
            return Self::zero();
        }
        Self(self.0 + rhs.0)
    }
}

impl std::ops::Div for ExpNum {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        if self.is_zero() {
            return self;
        }
        if rhs.is_zero() {
            panic!("divide by zero");
        }
        Self(self.0 - rhs.0)
    }
}

impl std::fmt::Display for ExpNum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "e^{}", self.0)
    }
}

impl std::ops::AddAssign for ExpNum {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl std::ops::SubAssign for ExpNum {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl std::ops::MulAssign for ExpNum {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}

impl std::ops::DivAssign for ExpNum {
    fn div_assign(&mut self, rhs: Self) {
        *self = *self / rhs;
    }
}

impl PartialEq for ExpNum {
    fn eq(&self, other: &Self) -> bool {
        if self.is_zero() {
            return other.is_zero();
        }
        self.0 == other.0
    }
}

impl Eq for ExpNum {}

impl PartialOrd for ExpNum {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.is_zero() && other.is_zero() {
            return Some(std::cmp::Ordering::Equal);
        }
        self.0.partial_cmp(&other.0)
    }
}

#[cfg(test)]
mod test_expnum {
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

        let a = ExpNum::from(0.);
        let b = ExpNum::from(0.);
        assert!(f64::from(a + b) < 1e-9);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExpNumSign { Positive, Negative }

#[derive(Debug, Clone, Copy, PartialEq)]
struct SignedExpNum {
    sign: ExpNumSign,
    magnitude: ExpNum,
}

impl SignedExpNum {
    fn positive(magnitude: ExpNum) -> Self {
        Self { sign: ExpNumSign::Positive, magnitude }
    }

    fn positive_zero() -> Self {
        Self { sign: ExpNumSign::Positive, magnitude: ExpNum::zero() }
    }

    fn negative(magnitude: ExpNum) -> Self {
        Self { sign: ExpNumSign::Negative, magnitude }
    }

    fn negative_zero() -> Self {
        Self { sign: ExpNumSign::Negative, magnitude: ExpNum::zero() }
    }

    fn format_with_preference(&self, preference: &[impl Borrow<str>], output_type: &str) -> String {
        assert_eq!(output_type, "html");

        match self.sign {
            ExpNumSign::Positive => format!("+{}", self.magnitude.format_with_preference(preference, output_type)),
            ExpNumSign::Negative => format!("-{}", self.magnitude.format_with_preference(preference, output_type)),
        }
    }
}

impl std::ops::Add for SignedExpNum {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self.sign, rhs.sign) {
            (ExpNumSign::Positive, ExpNumSign::Positive) => {
                Self::positive(self.magnitude + rhs.magnitude)
            }
            (ExpNumSign::Positive, ExpNumSign::Negative) => {
                if self.magnitude > rhs.magnitude {
                    Self::positive(self.magnitude - rhs.magnitude)
                } else {
                    Self::negative(rhs.magnitude - self.magnitude)
                }
            }
            (ExpNumSign::Negative, ExpNumSign::Positive) => {
                if self.magnitude > rhs.magnitude {
                    Self::negative(self.magnitude - rhs.magnitude)
                } else {
                    Self::positive(rhs.magnitude - self.magnitude)
                }
            }
            (ExpNumSign::Negative, ExpNumSign::Negative) => {
                Self::negative(self.magnitude + rhs.magnitude)
            }
        }
    }
}

impl std::ops::AddAssign for SignedExpNum {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl std::ops::Sub for SignedExpNum {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self + (-rhs)
    }
}

impl std::ops::Neg for SignedExpNum {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self.sign {
            ExpNumSign::Positive => Self::negative(self.magnitude),
            ExpNumSign::Negative => Self::positive(self.magnitude),
        }
    }
}

impl std::ops::Mul for SignedExpNum {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self.sign, rhs.sign) {
            (ExpNumSign::Positive, ExpNumSign::Positive) => {
                Self::positive(self.magnitude * rhs.magnitude)
            }
            (ExpNumSign::Positive, ExpNumSign::Negative) => {
                Self::negative(self.magnitude * rhs.magnitude)
            }
            (ExpNumSign::Negative, ExpNumSign::Positive) => {
                Self::negative(self.magnitude * rhs.magnitude)
            }
            (ExpNumSign::Negative, ExpNumSign::Negative) => {
                Self::positive(self.magnitude * rhs.magnitude)
            }
        }
    }
}

impl std::ops::Div for SignedExpNum {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self.sign, rhs.sign) {
            (ExpNumSign::Positive, ExpNumSign::Positive) => {
                Self::positive(self.magnitude / rhs.magnitude)
            }
            (ExpNumSign::Positive, ExpNumSign::Negative) => {
                Self::negative(self.magnitude / rhs.magnitude)
            }
            (ExpNumSign::Negative, ExpNumSign::Positive) => {
                Self::negative(self.magnitude / rhs.magnitude)
            }
            (ExpNumSign::Negative, ExpNumSign::Negative) => {
                Self::positive(self.magnitude / rhs.magnitude)
            }
        }
    }
}

impl From<f64> for SignedExpNum {
    fn from(x: f64) -> Self {
        if x >= 0. {
            Self::positive(ExpNum::from(x))
        } else {
            Self::negative(ExpNum::from(-x))
        }
    }
}

impl<T: Borrow<Self>> std::iter::Sum<T> for SignedExpNum {
    fn sum<I: Iterator<Item = T>>(iter: I) -> Self {
        iter.fold(SignedExpNum::positive_zero(), |acc, x| acc + *x.borrow())
    }
}

macro_rules! new_usize_type {
    ($visibility: vis, $type_name: ident) => {
        #[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
        #[repr(transparent)]
        $visibility struct $type_name(pub usize);
        impl<T: Into<$type_name>> std::ops::Add<T> for $type_name {
            type Output = $type_name;

            fn add(self, rhs: T) -> $type_name {
                $type_name(self.0 + rhs.into().0)
            }
        }

        impl<T: Into<$type_name>> std::ops::AddAssign<T> for $type_name {
            fn add_assign(&mut self, rhs: T) {
                self.0 += rhs.into().0;
            }
        }

        impl From<usize> for $type_name {
            fn from(x: usize) -> $type_name {
                $type_name(x)
            }
        }

        impl std::fmt::Display for $type_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }
    }
}

new_usize_type!(pub, ResourceId);

struct Resource {
    name: &'static str,
    display_name: JsonValue,
}

new_usize_type!(pub, BuildingId);

struct Building {
    name: &'static str,
    display_name: JsonValue,
    max_level: u32,
    detail: JsonValue
}

new_usize_type!(pub, RelicId);

struct Relic {
    name: &'static str,
    display_name: JsonValue,
    passive: bool,
    detail: JsonValue
}

/// simple serializable enum value
#[derive(Clone, Copy)]
enum SValue {
    Int(u32),
    Num(ExpNum),
    SignedNum(SignedExpNum),
    Bool(bool),
    None
}

impl SValue {
    fn as_int(&self) -> u32 {
        match self {
            SValue::Int(x) => *x,
            _ => unreachable!()
        }
    }

    fn as_num(&self) -> ExpNum {
        match self {
            SValue::Num(x) => *x,
            _ => unreachable!()
        }
    }

    fn as_signed_num(&self) -> SignedExpNum {
        match self {
            SValue::SignedNum(x) => *x,
            _ => unreachable!()
        }
    }

    fn as_bool(&self) -> bool {
        match self {
            SValue::Bool(x) => *x,
            _ => unreachable!()
        }
    }

    fn is_none(&self) -> bool {
        match self {
            SValue::None => true,
            _ => false
        }
    }
}

impl ToString for SValue {
    fn to_string(&self) -> String {
        match self {
            SValue::Int(x) => format!("i{x}"),
            SValue::Num(x) => if x.is_zero() {
                "0".to_string()
            } else {
                format!("n{}", x.as_exp())
            },
            SValue::SignedNum(x) => if x.magnitude.is_zero() {
                "±0".to_string()
            } else {
                match x.sign {
                    ExpNumSign::Positive => format!("+{}", x.magnitude.as_exp()),
                    ExpNumSign::Negative => format!("-{}", x.magnitude.as_exp()),
                }
            },
            SValue::Bool(x) => match x {
                true => "t".to_string(),
                false => "f".to_string()
            },
            SValue::None => "∅".to_string()
        }
    }
}

impl FromStr for SValue {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.chars().next().unwrap() {
            'i' => Ok(SValue::Int(s[1..].parse().unwrap())),
            '0' => Ok(SValue::Num(ExpNum::zero())),
            'n' => Ok(SValue::Num(ExpNum::from_exp(s[1..].parse::<f64>().unwrap()))),
            '±' => Ok(SValue::SignedNum(SignedExpNum::positive_zero())),
            '+' => Ok(SValue::SignedNum(SignedExpNum::positive(ExpNum::from_exp(s[1..].parse::<f64>().unwrap())))),
            '-' => Ok(SValue::SignedNum(SignedExpNum::negative(ExpNum::from_exp(s[1..].parse::<f64>().unwrap())))),
            't' => Ok(SValue::Bool(true)),
            'f' => Ok(SValue::Bool(false)),
            '∅' => Ok(SValue::None),
            _ => Err(())
        }
    }
}

struct GameDef {
    resources: Vec<Resource>,
    buildings: Vec<Building>,
    relics: Vec<Relic>,
    handlers: BTreeMap<String, Vec<Box<dyn Fn(&mut Game, &JsonValue)>>>
}

impl Index<ResourceId> for GameDef {
    type Output = Resource;

    fn index(&self, index: ResourceId) -> &Self::Output {
        &self.resources[index.0]
    }
}

impl Index<BuildingId> for GameDef {
    type Output = Building;

    fn index(&self, index: BuildingId) -> &Self::Output {
        &self.buildings[index.0]
    }
}

impl Index<RelicId> for GameDef {
    type Output = Relic;

    fn index(&self, index: RelicId) -> &Self::Output {
        &self.relics[index.0]
    }
}

impl GameDef {
    const fn new() -> Self {
        Self {
            resources: Vec::new(),
            buildings: Vec::new(),
            relics: Vec::new(),
            handlers: BTreeMap::new()
        }
    }

    fn add_handler(&mut self, name: impl ToString, handler: Box<dyn Fn(&mut Game, &JsonValue)>) {
        self.handlers.entry(name.to_string()).or_default().push(handler);
    }

    fn is_initialized(&self) -> bool {
        !self.resources.is_empty()
    }
}

static mut GAME_DEF: GameDef = GameDef::new();

macro_rules! game_def {
    () => {
        unsafe { &GAME_DEF }
    };
    ($id: expr) => {
        unsafe { &GAME_DEF[$id] }
    }
}

fn init_game_def() {
    let game_def = unsafe { &mut GAME_DEF } ;

    // ensure to run as the first handler for step
    game_def.add_handler("step", Box::new(move |game, _| {
        game.day += 1;

        for i in 0..game.incomes.len() {
            for j in 0..game.incomes[i].len() {
                game.incomes[i][j][game.day as usize % 10] = SignedExpNum::positive_zero();
            }
        }
    }));

    game_def.add_handler("checkpoint", Box::new(move |_game, _| {
        panic!("checkpoint event should not be dispatched!");
    }));

    game_def.add_handler("persistent_state", Box::new(move |game, persistent_state| {
        game.load_persistent_state(persistent_state)
    }));

    game_def.add_handler("init", Box::new(move |game, _| {
        game.post_message_to_front("init", json!({
            "resource_defs": game_def!().resources.iter().map(|r| json!({
                "name": r.name,
                "display_name": r.display_name,
            })).collect::<Vec<_>>(),
    
            "building_defs": game_def!().buildings.iter().map(|b| json!({
                "name": b.name,
                "display_name": b.display_name,
                "max_level": b.max_level,
                "detail": b.detail
            })).collect::<Vec<_>>(),

            "relic_defs": game_def!().relics.iter().map(|r| json!({
                "name": r.name,
                "display_name": r.display_name,
                "passive": r.passive,
                "detail": r.detail
            })).collect::<Vec<_>>(),
        }));
    }));

    game_def.add_handler("format_preference", Box::new(move |game, format_preference| {
        game.format_preference = format_preference.as_array().unwrap().iter().map(|x| x.as_str().unwrap().to_string()).collect();
        game.post_resources();
    }));

    game_def.add_handler("income_preference", Box::new(move |game, income_preference| {
        game.income_preference = income_preference.as_u64().unwrap() as _;
        game.post_incomes();
    }));

    let resource_man_id = ResourceId(game_def.resources.len());
    game_def.resources.push(Resource {
        name: "man",
        display_name: json!({
            "en": "Manpower",
            "zh": "人力",
        })
    });

    let resource_metal_id = ResourceId(game_def.resources.len());
    game_def.resources.push(Resource {
        name: "metal",
        display_name: json!({
            "en": "Metal",
            "zh": "金",
        })
    });

    let resource_wood_id = ResourceId(game_def.resources.len());
    game_def.resources.push(Resource {
        name: "wood",
        display_name: json!({
            "en": "Wood",
            "zh": "木",
        })
    });

    let resource_water_id = ResourceId(game_def.resources.len());
    game_def.resources.push(Resource {
        name: "water",
        display_name: json!({
            "en": "Water",
            "zh": "水",
        })
    });

    let resource_fire_id = ResourceId(game_def.resources.len());
    game_def.resources.push(Resource {
        name: "fire",
        display_name: json!({
            "en": "Fire",
            "zh": "火",
        })
    });

    let resource_earth_id = ResourceId(game_def.resources.len());
    game_def.resources.push(Resource {
        name: "earth",
        display_name: json!({
            "en": "Earth",
            "zh": "土",
        })
    });

    let resource_yang_id = ResourceId(game_def.resources.len());
    game_def.resources.push(Resource {
        name: "yang",
        display_name: json!({
            "en": "Yang", // or Light & Dark?
            "zh": "阳",
        })
    });

    let resource_yin_id = ResourceId(game_def.resources.len());
    game_def.resources.push(Resource {
        name: "yin",
        display_name: json!({
            "en": "Yin",
            "zh": "阴",
        })
    });



    fn add_relic(
        game_def: &mut GameDef,
        relic: Relic,
        forge_condition_fun: Rc<dyn Fn(&Game) -> bool>,
        forge_effect_fun: Rc<dyn Fn(&mut Game)>,
        activate_condition_fun: Rc<dyn Fn(&Game) -> bool>,
        activate_effect_fun: Rc<dyn Fn(&mut Game)>,
        cooldown_fun: Rc<dyn Fn(&Game) -> u32>,
        detail_fun: Rc<dyn Fn(&Game) -> JsonValue>
    ) -> RelicId {
        let name = relic.name;
        let passive = relic.passive;

        let id = RelicId(game_def.relics.len());
        game_def.relics.push(relic);

        game_def.add_handler(format!("{name}.forge"), {
            let forge_condition_fun = forge_condition_fun.clone();
            let forge_effect_fun = forge_effect_fun.clone();

            Box::new(move |game, _| {
                if !game[id].get("unlocked").map_or(false, |x| x.as_bool()) || game[id].get("forged").map_or(false, |x| x.as_bool()) {
                    return
                }

                if !forge_condition_fun(game) {
                    game.post_message_to_front("log", json!({
                        "en": format!("Cannot forge {}", game_def!(id).display_name["en"].as_str().unwrap()),
                        "zh": format!("无法锻造{}", game_def!(id).display_name["zh"].as_str().unwrap())
                    }));
                    return
                }

                forge_effect_fun(game);

                game[id].insert("forged".to_string(), SValue::Bool(true));
                game.post_relic_properties(id);
            })
        });

        game_def.add_handler(format!("{name}.devforge"), {
            let forge_effect_fun = forge_effect_fun.clone();

            Box::new(move |game, _| {
                forge_effect_fun(game);
                game[id].insert("forged".to_string(), SValue::Bool(true));
                game.post_relic_properties(id);
            })
        });

        game_def.add_handler(format!("{name}.activate"), {
            let activate_condition_fun = activate_condition_fun.clone();
            let cooldown_fun = cooldown_fun.clone();

            Box::new(move |game, _| {
                if !game[id].get("forged").map_or(false, |x| x.as_bool()) || passive {
                    return
                }

                if game[id].get("cooldown_at").map_or(0, |x| x.as_int()) > game.day {
                    game.post_message_to_front("log", json!({
                        "en": format!("{} is still cooling down.", game_def!(id).display_name["en"].as_str().unwrap()),
                        "zh": format!("{}还在冷却中。", game_def!(id).display_name["zh"].as_str().unwrap())
                    }));
                    return
                }

                if !activate_condition_fun(game) {
                    game.post_message_to_front("log", json!({
                        "en": format!("Cannot activate {}", game_def!(id).display_name["en"].as_str().unwrap()),
                        "zh": format!("无法激活{}", game_def!(id).display_name["zh"].as_str().unwrap())
                    }));
                    return
                }

                activate_effect_fun(game);

                let cooldown_time = game.day + cooldown_fun(game);
                game[id].insert("cooldown_at".to_string(), SValue::Int(cooldown_time));
                game.post_relic_properties(id);
            })
        });

        game_def.add_handler(format!("{name}.detail"), {
            let forge_condition_fun = forge_condition_fun.clone();
            let activate_condition_fun = activate_condition_fun.clone();
            let cooldown_fun = cooldown_fun.clone();

            Box::new(move |game, _| {
                let mut details = detail_fun(game);
                details["forgable"] = forge_condition_fun(game).into();
                details["activatable"] = activate_condition_fun(game).into();
                details["cooldown"] = cooldown_fun(game).into();
                game.post_message_to_front(&format!("{name}.detail"), details);
            })
        });

        id
    }


    let relic_coupon_id = RelicId(game_def.relics.len());
    add_relic(game_def, Relic {
        name: "coupon",
        display_name: json!({
            "en": "Kids Coupon",
            "zh": "儿童优惠券",
        }),
        passive: true,
        detail: json!({
            "description": {
                "en": r#"50% discount for all building and upgrading costs before Day 6570."#,
                "zh": r#"在第6570天前，所有建筑建造和升级的成本减半。"#,
            },
            "forge_condition": {
                "en": r#"<ca-relic-detail-slot>forge_cost.metal</ca-relic-detail-slot> <ca-resource>metal</ca-resource>"#,
                "zh": r#"<ca-relic-detail-slot>forge_cost.metal</ca-relic-detail-slot><ca-resource>metal</ca-resource>"#,
            },
            "activate_condition": {
                "en": r#""#,
                "zh": r#""#,
            },
            "cooldown": {
                "en": r#""#,
                "zh": r#""#,
            }
        }),
    },
    // forge_condition_fun
    Rc::new(move |game| game[resource_metal_id] >= ExpNum::from_exp(20.)),
    // forge_effect_fun
    Rc::new(move |game| {
        // no need to pay the cost; the user can always warp one day back to reclaim the expense while keeping the relic.
        game.post_message_to_front("building.details", JsonNull) // ask front end to update building details
    }),
    // activate_condition_fun
    Rc::new(move |_game| false),
    // activate_effect_fun
    Rc::new(move |_game| {}),
    // cooldown_fun
    Rc::new(move |_game| 0),
    // detail_fun
    Rc::new(move |game| json!({
        "forge_cost.metal": ExpNum::from_exp(20.).format_with_preference(&game.format_preference, "html")
    })));

    let wrap_cost_fun_with_relic_coupon = |cost_fun: Rc<dyn Fn(&Game) -> Vec<(ResourceId, ExpNum)>>| -> Rc<dyn Fn(&Game) -> Vec<(ResourceId, ExpNum)>> {
        Rc::new(move |game| {
            let mut costs = cost_fun(game);
            if game[relic_coupon_id].get("forged").map_or(false, |x| x.as_bool()) && game.day <= 6570 {
                for (_, cost) in costs.iter_mut() {
                    *cost /= ExpNum::from(2.);
                }
            }
            costs
        })
    };

    game_def.add_handler("init", Box::new(move |game, _| {
        game[relic_coupon_id].insert("unlocked".to_string(), SValue::Bool(true));
        game.post_relic_properties(relic_coupon_id);
    }));


    let relic_pstone_id = RelicId(game_def.relics.len());
    add_relic(game_def, Relic {
        name: "pstone",
        display_name: json!({
            "en": "Philosopher's Stone",
            "zh": "贤者之石",
        }),
        passive: false,
        detail: json!({
            "description": {
                "en": r#"Converts half of <ca-resource>man</ca-resource> into the least abundant resource among <ca-resource>metal</ca-resource>, <ca-resource>wood</ca-resource>, <ca-resource>water</ca-resource>, <ca-resource>fire</ca-resource>, and <ca-resource>earth</ca-resource>."#,
                "zh": r#"将一半的<ca-resource>man</ca-resource>转化为<ca-resource>metal</ca-resource><ca-resource>wood</ca-resource><ca-resource>water</ca-resource><ca-resource>fire</ca-resource><ca-resource>earth</ca-resource>中最少的资源。"#,
            },
            "forge_condition": {
                "en": r#"<ca-relic-detail-slot>forge_cost.man</ca-relic-detail-slot> <ca-resource>man</ca-resource>"#,
                "zh": r#"<ca-relic-detail-slot>forge_cost.man</ca-relic-detail-slot><ca-resource>man</ca-resource>"#,
            },
            "activate_condition": {
                "en": r#""#,
                "zh": r#""#,
            },
            "cooldown": {
                "en": r#"<ca-katex>1 + \lfloor\text{{Day}}^{{0.8}}\rfloor</ca-katex> = <ca-relic-detail-slot>cooldown</ca-relic-detail-slot> Days"#,
                "zh": r#"<ca-katex>1 + \lfloor\text{{\footnotesize 天数}}^{{0.8}}\rfloor</ca-katex> = <ca-relic-detail-slot>cooldown</ca-relic-detail-slot>天"#,
            }
        }),
    },
    // forge_condition_fun
    Rc::new(move |game| game[resource_man_id] >= ExpNum::from_exp(20.)),
    // forge_effect_fun
    Rc::new(move |_game| {}),
    // activate_condition_fun
    Rc::new(move |_game| true),
    // activate_effect_fun
    Rc::new(move |game| {
        let least_owned_resource_id = [
            resource_metal_id,
            resource_wood_id,
            resource_water_id,
            resource_fire_id,
            resource_earth_id,
        ].into_iter().min_by(|&x, &y| game[x].partial_cmp(&game[y]).unwrap()).unwrap();

        let half_man = game[resource_man_id] / ExpNum::from(2.);
        game[resource_man_id] -= half_man;
        game[least_owned_resource_id] += half_man;
    }),
    // cooldown_fun
    Rc::new(move |game| 1 + (game.day as f64).powf(0.8).floor() as u32),
    // detail_fun
    Rc::new(move |game| json!({
        "forge_cost.man": ExpNum::from_exp(20.).format_with_preference(&game.format_preference, "html"),
        "cooldown": 1 + (game.day as f64).powf(0.8).floor() as u32,
    })));

    game_def.add_handler("smelter.built", Box::new(move |game, _| {
        if game[relic_pstone_id].get("unlocked").map_or(false, |x| x.as_bool()) {
            return;
        }
        game[relic_pstone_id].insert("unlocked".to_string(), SValue::Bool(true));
        game.post_relic_properties(relic_pstone_id);
    }));




    let add_building = |
        game_def: &mut GameDef,
        building: Building,
        cost_fun: Rc<dyn Fn(&Game) -> Vec<(ResourceId, ExpNum)>>,
        product_fun: Rc<dyn Fn(&Game) -> Vec<(ResourceId, ExpNum)>>,
        upgrade_cost_fun: Rc<dyn Fn(&Game) -> Vec<(ResourceId, ExpNum)>>,
        build_cost_fun: Rc<dyn Fn(&Game) -> Vec<(ResourceId, ExpNum)>>
    | -> BuildingId {
        let name = building.name;
        let max_level = building.max_level;

        let id = BuildingId(game_def.buildings.len());
        game_def.buildings.push(building);

        let upgrade_cost_fun = wrap_cost_fun_with_relic_coupon(upgrade_cost_fun);
        let build_cost_fun = wrap_cost_fun_with_relic_coupon(build_cost_fun);

        game_def.add_handler("step".to_string(), {
            let cost_fun = cost_fun.clone();
            let product_fun = product_fun.clone();

            Box::new(move |game, _| {
                if !game[id].get("enabled").map_or(false, |x| x.as_bool()) {
                    return;
                }

                let costs = cost_fun(game);

                for &(resource_id, cost) in &costs {
                    if game[resource_id] < cost {
                        game[id].insert("enabled".to_string(), SValue::Bool(false));
                        game.post_building_properties(id);
                        game.post_message_to_front("log", json!({
                            "en": format!("{} is automatically disabled due to lack of resources", game_def!(id).display_name["en"].as_str().unwrap()),
                            "zh": format!("{}资源不足，已自动停用", game_def!(id).display_name["zh"].as_str().unwrap())
                        }));
                        return;
                    }
                }

                for (resource_id, cost) in costs {
                    game[resource_id] -= cost;
                    game.record_income(resource_id, id, SignedExpNum::negative(cost));
                }

                let products = product_fun(game);

                for &(resource_id, amount) in &products {
                    game[resource_id] += amount;
                    game.record_income(resource_id, id, SignedExpNum::positive(amount));
                }
            })
        });

        game_def.add_handler(format!("{name}.build"), {
            let build_cost_fun = build_cost_fun.clone();

            Box::new(move |game, _| {
                if !game[id].get("unlocked").map_or(false, |x| x.as_bool()) {
                    return;
                }

                let costs = build_cost_fun(game);

                for &(resource_id, cost) in &costs {
                    if game[resource_id] < cost {
                        game.post_message_to_front("log", json!({
                            "en": format!("Not enough {} to build {}", game_def!(resource_id).display_name["en"].as_str().unwrap(), game_def!(id).display_name["en"].as_str().unwrap()),
                            "zh": format!("{}不足，无法建造{}", game_def!(resource_id).display_name["zh"].as_str().unwrap(), game_def!(id).display_name["zh"].as_str().unwrap())
                        }));
                        return;
                    }
                }

                for (resource_id, cost) in costs {
                    game[resource_id] -= cost;
                }

                game[id].insert("built".to_string(), SValue::Bool(true));
                game[id].insert("enabled".to_string(), SValue::Bool(true));
                game[id].insert("level".to_string(), SValue::Int(1));
                game.post_building_properties(id);
                game.dispatch_message(&format!("{name}.built"), &JsonNull);
            })
        });

        game_def.add_handler(format!("{name}.enable"), Box::new(move |game, _| {
            if !game[id].get("built").map_or(false, |x| x.as_bool()) {
                return;
            }

            game[id].insert("enabled".to_string(), SValue::Bool(true));
            game.post_building_properties(id);
        }));

        game_def.add_handler(format!("{name}.disable"), Box::new(move |game, _| {
            if !game[id].get("built").map_or(false, |x| x.as_bool()) {
                return;
            }

            game[id].insert("enabled".to_string(), SValue::Bool(false));
            game.post_building_properties(id);
        }));

        game_def.add_handler(format!("{name}.upgrade"), {
            let upgrade_cost_fun = upgrade_cost_fun.clone();

            Box::new(move |game, _| {
                if !game[id].get("built").map_or(false, |x| x.as_bool()) {
                    return
                }

                let current_level = game[id]["level"].as_int();
                if current_level >= max_level {
                    return
                }

                let costs = upgrade_cost_fun(game);

                for &(resource_id, cost) in &costs {
                    if game[resource_id] < cost {
                        game.post_message_to_front("log", json!({
                            "en": format!("Not enough {} to upgrade {}", game_def!(resource_id).display_name["en"].as_str().unwrap(), game_def!(id).display_name["en"].as_str().unwrap()),
                            "zh": format!("{}不足，无法升级{}", game_def!(resource_id).display_name["zh"].as_str().unwrap(), game_def!(id).display_name["zh"].as_str().unwrap())
                        }));
                        return;
                    }
                }

                for (resource_id, cost) in costs {
                    game[resource_id] -= cost;
                }

                game[id].insert("level".to_string(), SValue::Int(current_level + 1));
                game.post_building_properties(id);
                game.dispatch_message(&format!("{name}.upgraded"), &JsonNull);
            })
        });

        game_def.add_handler(format!("{name}.devupgrade"), Box::new(move |game, _| {
            let current_level = game[id]["level"].as_int();
            if current_level >= max_level {
                return
            }
            game[id].insert("level".to_string(), SValue::Int(current_level + 1));
            game.post_building_properties(id);
            game.dispatch_message(&format!("{name}.upgraded"), &JsonNull);
        }));

        game_def.add_handler(format!("{name}.downgrade"), Box::new(move |game, _| {
            if !game[id].get("built").map_or(false, |x| x.as_bool()) {
                return;
            }

            let current_level = game[id]["level"].as_int();
            if current_level == 1 {
                return;
            }

            game[id].insert("level".to_string(), SValue::Int(current_level - 1));
            game.post_building_properties(id);
        }));

        game_def.add_handler(format!("{name}.detail"), {
            let cost_fun = cost_fun.clone();
            let product_fun = product_fun.clone();
            let build_cost_fun = build_cost_fun.clone();
            let upgrade_cost_fun = upgrade_cost_fun.clone();

            Box::new(move |game, _| {
                // getting details use some information that is may not always available
                if !game[id].get("unlocked").map_or(false, |x| x.as_bool()) {
                    return;
                }

                let mut detail = json!({
                    "cost": {},
                    "product": {},
                    "build_cost": {},
                    "upgrade_cost": {}
                });

                if game[id].get("built").map_or(false, |x| x.as_bool()) {
                    for (resource_id, amount) in cost_fun(game) {
                        let name = game_def!(resource_id).name;
                        let sufficient = game[resource_id] >= amount;
                        let amount = amount.format_with_preference(&game.format_preference, "html");
                        detail["cost"][name] = json!([amount, sufficient]);
                    }

                    for (resource_id, amount) in product_fun(game) {
                        let name = game_def!(resource_id).name;
                        let amount = amount.format_with_preference(&game.format_preference, "html");
                        detail["product"][name] = json!([amount, true]);
                    }

                    if game[id]["level"].as_int() < max_level {
                        for (resource_id, amount) in upgrade_cost_fun(game) {
                            let name = game_def!(resource_id).name;
                            let sufficient = game[resource_id] >= amount;
                            let amount = amount.format_with_preference(&game.format_preference, "html");
                            detail["upgrade_cost"][name] = json!([amount, sufficient]);
                        }
                    }
                } else {
                    for (resource_id, amount) in build_cost_fun(game) {
                        let name = game_def!(resource_id).name;
                        let sufficient = game[resource_id] >= amount;
                        let amount = amount.format_with_preference(&game.format_preference, "html");
                        detail["build_cost"][name] = json!([amount, sufficient]);
                    }
                }

                game.post_message_to_front(&format!("{name}.detail"), detail);
            })
        });

        id
    };


    let building_tent_id = BuildingId(game_def.buildings.len());
    add_building(game_def, Building {
        name: "tent",
        display_name: json!({
            "en": "Tent",
            "zh": "帐篷",
        }),
        max_level: 10,
        detail: json!({
            "description": {
                "en": r#"A tent. Generates <ca-resource>man</ca-resource>."#,
                "zh": r#"一个帐篷。生成<ca-resource>man</ca-resource>。"#,
            },
            "cost": {
                "en": r#""#,
                "zh": r#""#,
            },
            "product": {
                "en": r#"<ca-katex>2 ^ {{\text{{level}} - 1}}</ca-katex> = <ca-building-detail-slot>product.man</ca-building-detail-slot> <ca-resource>man</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{\tiny 等级}} - 1}}</ca-katex> = <ca-building-detail-slot>product.man</ca-building-detail-slot> <ca-resource>man</ca-resource>"#,
            }
        })
    },
    // cost
    Rc::new(move |_| vec![]),
    // product
    Rc::new(move |game| {
        let level = game[building_tent_id]["level"].as_int();
        vec![(resource_man_id, ExpNum::from(2.).pow(level - 1))]
    }),
    // upgrade cost
    Rc::new(move |game| {
        let level = game[building_tent_id]["level"].as_int();
        if level < 5 {
            return vec![(resource_man_id, ExpNum::from(2.).pow(level + 1))];
        }

        vec![
            (resource_wood_id, ExpNum::from(2.).pow(level)),
            (resource_earth_id, ExpNum::from(1.5).pow(level)),
        ]
    }),
    // build cost
    Rc::new(move |_| {
        vec![]
    }));

    game_def.add_handler("init", Box::new(move |game, _| {
        game[building_tent_id].insert("unlocked".to_string(), SValue::Bool(true));
        game[building_tent_id].insert("built".to_string(), SValue::Bool(true));
        game[building_tent_id].insert("enabled".to_string(), SValue::Bool(true));
        game[building_tent_id].insert("level".to_string(), SValue::Int(1));
        game.post_building_properties(building_tent_id);
    }));


    let building_forest_id = BuildingId(game_def.buildings.len());
    add_building(game_def, Building {
        name: "forest",
        display_name: json!({
            "en": "Forest",
            "zh": "森林",
        }),
        max_level: 65536,
        detail: json!({
            "description": {
                "en": r#"A forest. Excessive <ca-resource>man</ca-resource> reduces <ca-resource>wood</ca-resource> production."#,
                "zh": r#"一片森林。过多<ca-resource>man</ca-resource>会减少<ca-resource>wood</ca-resource>产出。"#,
            },
            "cost": {
                "en": r#""#,
                "zh": r#""#,
            },
            "product": {
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 10}} - \text{{Manpower}} ^ {{0.8}}</ca-katex> = <ca-building-detail-slot>product.wood</ca-building-detail-slot> <ca-resource>wood</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{\tiny 等级}} + 10}} - \text{{\footnotesize 人力}} ^ {{0.8}}</ca-katex> = <ca-building-detail-slot>product.wood</ca-building-detail-slot> <ca-resource>wood</ca-resource>"#,
            }
        })
    },
    // cost
    Rc::new(move |_| vec![]),
    // product
    Rc::new(move |game| {
        let level = game[building_forest_id]["level"].as_int();
        let man = game[resource_man_id];
        let a = ExpNum::from(2.).pow(level + 10);
        let b = man.pow(0.8);
        let product = if a > b {
            a - b
        } else {
            ExpNum::from(0.)
        };
        vec![(resource_wood_id, product)]
    }),
    // upgrade cost
    Rc::new(move |game| {
        let level = game[building_forest_id]["level"].as_int();
        vec![
            (resource_water_id, ExpNum::from(2.2).pow(level + 8)),
            (resource_earth_id, ExpNum::from(2.).pow(level + 10)),
        ]
    }),
    // build cost
    Rc::new(move |_| vec![]));

    game_def.add_handler("tent.upgraded", Box::new(move |game, _| {
        if game[building_forest_id].get("unlocked").map_or(false, |x| x.as_bool()) {
            return;
        }
        let tent_level = game[building_tent_id]["level"].as_int();
        if tent_level >= 5 {
            game[building_forest_id].insert("unlocked".to_string(), SValue::Bool(true));
            game[building_forest_id].insert("built".to_string(), SValue::Bool(true));
            game[building_forest_id].insert("enabled".to_string(), SValue::Bool(true));
            game[building_forest_id].insert("level".to_string(), SValue::Int(1));
            game.post_building_properties(building_forest_id);
        }
    }));


    let building_swamp_id = BuildingId(game_def.buildings.len());
    add_building(game_def, Building {
        name: "swamp",
        display_name: json!({
            "en": "Swamp",
            "zh": "沼泽",
        }),
        max_level: 65536,
        detail: json!({
            "description": {
                "en": r#"Generates <ca-resource>water</ca-resource> and <ca-resource>earth</ca-resource> based on thier ratio."#,
                "zh": r#"根据<ca-resource>water</ca-resource>和<ca-resource>earth</ca-resource>的比例生成<ca-resource>water</ca-resource>和<ca-resource>earth</ca-resource>。"#,
            },
            "cost": {
                "en": r#""#,
                "zh": r#""#,
            },
            "product": {
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 8}} \times \frac{{\text{{Water}} + 1}}{{\text{{Water}} + \text{{Earth}} + 1}}</ca-katex> = <ca-building-detail-slot>product.water</ca-building-detail-slot> <ca-resource>water</ca-resource><br><ca-katex style="line-height: 2">2 ^ {{\text{{level}} + 8}} \times \frac{{\text{{Earth}} + 1}}{{\text{{Water}} + \text{{Earth}} + 1}}</ca-katex> = <ca-building-detail-slot>product.earth</ca-building-detail-slot> <ca-resource>earth</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{level}} + 8}} \times \frac{{\text{{水}} + 1}}{{\text{{水}} + \text{{土}} + 1}}</ca-katex> = <ca-building-detail-slot>product.water</ca-building-detail-slot> <ca-resource>water</ca-resource><br><ca-katex style="line-height: 2">2 ^ {{\text{{level}} + 8}} \times \frac{{\text{{土}} + 1}}{{\text{{水}} + \text{{土}} + 1}}</ca-katex> = <ca-building-detail-slot>product.earth</ca-building-detail-slot> <ca-resource>earth</ca-resource>"#,
            }
        })
    },
    // cost
    Rc::new(move |_| vec![]),
    // product
    Rc::new(move |game| {
        let level = game[building_swamp_id]["level"].as_int();
        let water = game[resource_water_id];
        let earth = game[resource_earth_id];
        
        let coeff = ExpNum::from(2.).pow(level + 8);

        let water_production = coeff * (water + ExpNum::from(1.)) / (water + earth + ExpNum::from(1.));
        let earth_production = coeff * (earth + ExpNum::from(1.)) / (water + earth + ExpNum::from(1.));
        vec![
            (resource_water_id, water_production),
            (resource_earth_id, earth_production)
        ]
    }),
    // upgrade cost
    Rc::new(move |game| {
        let level = game[building_swamp_id]["level"].as_int();
        vec![
            (resource_water_id, ExpNum::from(2.4).pow(level+9)),
            (resource_earth_id, ExpNum::from(2.4).pow(level+9)),
        ]
    }),
    // build cost
    Rc::new(move |_| vec![]));

    game_def.add_handler("tent.upgraded", Box::new(move |game, _| {
        if game[building_swamp_id].get("unlocked").map_or(false, |x| x.as_bool()) {
            return;
        }
        let tent_level = game[building_tent_id]["level"].as_int();
        if tent_level >= 5 {
            game[building_swamp_id].insert("unlocked".to_string(), SValue::Bool(true));
            game[building_swamp_id].insert("built".to_string(), SValue::Bool(true));
            game[building_swamp_id].insert("enabled".to_string(), SValue::Bool(true));
            game[building_swamp_id].insert("level".to_string(), SValue::Int(1));
            game.post_building_properties(building_swamp_id);
        }
    }));


    let building_campfire_id = BuildingId(game_def.buildings.len());
    add_building(game_def, Building {
        name: "campfire",
        display_name: json!({
            "en": "Campfire",
            "zh": "营火",
        }),
        max_level: 65536,
        detail: json!({
            "description": {
                "en": r#"Consumes <ca-resource>wood</ca-resource> to produce <ca-resource>fire</ca-resource>"#,
                "zh": r#"消耗<ca-resource>wood</ca-resource>产生<ca-resource>fire</ca-resource>"#,
            },
            "cost": {
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 8}}</ca-katex> = <ca-building-detail-slot>cost.wood</ca-building-detail-slot> <ca-resource>wood</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{\tiny 等级}} + 8}}</ca-katex> = <ca-building-detail-slot>cost.wood</ca-building-detail-slot> <ca-resource>wood</ca-resource>"#,
            },
            "product": {
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 9}}</ca-katex> = <ca-building-detail-slot>product.fire</ca-building-detail-slot> <ca-resource>fire</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{\tiny 等级}} + 9}}</ca-katex> = <ca-building-detail-slot>product.fire</ca-building-detail-slot> <ca-resource>fire</ca-resource>"#,
            }
        })
    },
    // cost
    Rc::new(move |game| {
        let level = game[building_campfire_id]["level"].as_int();
        vec![(resource_wood_id, ExpNum::from(2.).pow(level+8))]
    }),
    // product
    Rc::new(move |game| {
        let level = game[building_campfire_id]["level"].as_int();
        vec![(resource_fire_id, ExpNum::from(2.).pow(level+9))]
    }),
    // upgrade cost
    Rc::new(move |game| {
        let level = game[building_campfire_id]["level"].as_int();
        vec![(resource_man_id, ExpNum::from(2.2).pow(level+7))]
    }),
    // build cost
    Rc::new(move |_| vec![(resource_man_id, ExpNum::from(10000.))]));

    game_def.add_handler("tent.upgraded", Box::new(move |game, _| {
        if game[building_campfire_id].get("unlocked").map_or(false, |x| x.as_bool()) {
            return;
        }
        let tent_level = game[building_tent_id]["level"].as_int();
        if tent_level >= 5 {
            game[building_campfire_id].insert("unlocked".to_string(), SValue::Bool(true));
            game.post_building_properties(building_campfire_id);
        }
    }));



    let building_mine_id = BuildingId(game_def.buildings.len());
    add_building(game_def, Building {
        name: "mine",
        display_name: json!({
            "en": "Mine",
            "zh": "矿",
        }),
        max_level: 65536,
        detail: json!({
            "description": {
                "en": r#"Get <ca-resource>metal</ca-resource> with <ca-resource>man</ca-resource>"#,
                "zh": r#"消耗<ca-resource>man</ca-resource>产生<ca-resource>metal</ca-resource>"#,
            },
            "cost": {
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 5}}</ca-katex> = <ca-building-detail-slot>cost.man</ca-building-detail-slot> <ca-resource>man</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{\tiny 等级}} + 5}}</ca-katex> = <ca-building-detail-slot>cost.man</ca-building-detail-slot> <ca-resource>man</ca-resource>"#,
            },
            "product": {
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 6.5}}</ca-katex> = <ca-building-detail-slot>product.metal</ca-building-detail-slot> <ca-resource>metal</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{\tiny 等级}} + 6.5}}</ca-katex> = <ca-building-detail-slot>product.metal</ca-building-detail-slot> <ca-resource>metal</ca-resource>"#,
            }
        })
    },
    // cost
    Rc::new(move |game| {
        let level = game[building_mine_id]["level"].as_int();
        vec![(resource_man_id, ExpNum::from(2.).pow(level + 5))]
    }),
    // product
    Rc::new(move |game| {
        let level = game[building_mine_id]["level"].as_int();
        vec![(resource_metal_id, ExpNum::from(2.).pow(level as f64 + 6.5))]
    }),
    // upgrade cost
    Rc::new(move |game| {
        let level = game[building_mine_id]["level"].as_int();
        vec![(resource_man_id, ExpNum::from(2.2).pow(level + 8))]
    }),
    // build cost
    Rc::new(move |_| vec![(resource_man_id, ExpNum::from(10000.))]));

    game_def.add_handler("tent.upgraded", Box::new(move |game, _| {
        if game[building_mine_id].get("unlocked").map_or(false, |x| x.as_bool()) {
            return;
        }
        let tent_level = game[building_tent_id]["level"].as_int();
        if tent_level >= 5 {
            game[building_mine_id].insert("unlocked".to_string(), SValue::Bool(true));
            game.post_building_properties(building_mine_id);
        }
    }));



    let building_farm_id = BuildingId(game_def.buildings.len());
    add_building(game_def, Building {
        name: "Farm",
        display_name: json!({
            "en": "Farm",
            "zh": "田",
        }),
        max_level: 65536,
        detail: json!({
            "description": {
                "en": r#"A Farm"#,
                "zh": r#"一块田"#,
            },
            "cost": {
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 10}}</ca-katex> = <ca-building-detail-slot>cost.water</ca-building-detail-slot> <ca-resource>water</ca-resource><br><ca-katex>2 ^ {{\text{{level}}}}</ca-katex> = <ca-building-detail-slot>cost.earth</ca-building-detail-slot> <ca-resource>earth</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{\tiny 等级}} + 10}}</ca-katex> = <ca-building-detail-slot>cost.water</ca-building-detail-slot> <ca-resource>water</ca-resource><br><ca-katex>2 ^ {{\text{{\tiny 等级}}}}</ca-katex> = <ca-building-detail-slot>cost.earth</ca-building-detail-slot> <ca-resource>earth</ca-resource>"#,
            },
            "product": {
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 10}}</ca-katex> = <ca-building-detail-slot>product.man</ca-building-detail-slot> <ca-resource>man</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{\tiny 等级}} + 10}}</ca-katex> = <ca-building-detail-slot>product.man</ca-building-detail-slot> <ca-resource>man</ca-resource>"#,
            }
        })
    },
    // cost
    Rc::new(move |game| {
        let level = game[building_farm_id]["level"].as_int();
        vec![
            (resource_water_id, ExpNum::from(2.).pow(level + 10)),
            (resource_earth_id, ExpNum::from(2.).pow(level))
        ]
    }),
    // product
    Rc::new(move |game| {
        let level = game[building_farm_id]["level"].as_int();
        vec![(resource_man_id, ExpNum::from(2.).pow(level + 10))]
    }),
    // upgrade cost
    Rc::new(move |game| {
        let level = game[building_farm_id]["level"].as_int();
        vec![(resource_man_id, ExpNum::from(2.2).pow(level + 8))]
    }),
    // build cost
    Rc::new(move |_| vec![
        (resource_man_id, ExpNum::from(1000.)),
        (resource_water_id, ExpNum::from(1000.))
    ]));

    game_def.add_handler("tent.upgraded", Box::new(move |game, _| {
        if game[building_farm_id].get("unlocked").map_or(false, |x| x.as_bool()) {
            return;
        }
        let tent_level = game[building_tent_id]["level"].as_int();
        if tent_level >= 10 {
            game[building_farm_id].insert("unlocked".to_string(), SValue::Bool(true));
            game.post_building_properties(building_farm_id);
        }
    }));



    let building_nuke_id = BuildingId(game_def.buildings.len());
    add_building(game_def, Building {
        name: "nuke",
        display_name: json!({
            "en": "Nuclear Reactor",
            "zh": "核反应堆",
        }),
        max_level: 65536,
        detail: json!({
            "description": {
                "en": r#"That's how the sun work."#,
                "zh": r#"“阳”。"#,
            },
            "cost": {
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 10}}</ca-katex> = <ca-building-detail-slot>cost.metal</ca-building-detail-slot> <ca-resource>metal</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{\tiny 等级}} + 10}}</ca-katex> = <ca-building-detail-slot>cost.metal</ca-building-detail-slot> <ca-resource>metal</ca-resource>"#,
            },
            "product": {
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 9.5}}</ca-katex> = <ca-building-detail-slot>product.yang</ca-building-detail-slot> <ca-resource>yang</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{\tiny 等级}} + 9.5}}</ca-katex> = <ca-building-detail-slot>product.yang</ca-building-detail-slot> <ca-resource>yang</ca-resource>"#,
            }
        })
    },
    // cost
    Rc::new(move |game| {
        let level = game[building_nuke_id]["level"].as_int();
        vec![(resource_metal_id, ExpNum::from(2.).pow(level + 10))]
    }),
    // product
    Rc::new(move |game| {
        let level = game[building_nuke_id]["level"].as_int();
        vec![(resource_yang_id, ExpNum::from(2.).pow(level as f64 + 9.5))]
    }),
    // upgrade cost
    Rc::new(move |game| {
        let level = game[building_nuke_id]["level"].as_int();
        vec![
            (resource_metal_id, ExpNum::from(2.2).pow(level + 18)),
            (resource_water_id, ExpNum::from(1.8).pow(level + 20))
        ]
    }),
    // build cost
    Rc::new(move |_| vec![(resource_metal_id, ExpNum::from(2.).pow(16))]));

    game_def.add_handler("mine.upgraded", Box::new(move |game, _| {
        if game[building_nuke_id].get("unlocked").map_or(false, |x| x.as_bool()) {
            return;
        }
        let mine_level = game[building_mine_id]["level"].as_int();
        if mine_level >= 5 {
            game[building_nuke_id].insert("unlocked".to_string(), SValue::Bool(true));
            game.post_building_properties(building_nuke_id);
        }
    }));


    let building_smelter_id = BuildingId(game_def.buildings.len());
    add_building(game_def, Building {
        name: "smelter",
        display_name: json!({
            "en": "Smelter",
            "zh": "冶炼厂",
        }),
        max_level: 65536,
        detail: json!({
            "description": {
                "en": r#"A smelter."#,
                "zh": r#"炼金"#,
            },
            "cost": {
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 10}}</ca-katex> = <ca-building-detail-slot>cost.fire</ca-building-detail-slot> <ca-resource>fire</ca-resource><br><ca-katex>2 ^ {{\text{{level}} + 9}}</ca-katex> = <ca-building-detail-slot>cost.earth</ca-building-detail-slot> <ca-resource>earth</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{\tiny 等级}} + 10}}</ca-katex> = <ca-building-detail-slot>cost.fire</ca-building-detail-slot> <ca-resource>fire</ca-resource><br><ca-katex>2 ^ {{\text{{\tiny 等级}} + 9}}</ca-katex> = <ca-building-detail-slot>cost.earth</ca-building-detail-slot> <ca-resource>earth</ca-resource>"#,
            },
            "product": {
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 11}}</ca-katex> = <ca-building-detail-slot>product.metal</ca-building-detail-slot> <ca-resource>metal</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{\tiny 等级}} + 11}}</ca-katex> = <ca-building-detail-slot>product.metal</ca-building-detail-slot> <ca-resource>metal</ca-resource>"#,
            }
        })
    },
    // cost
    Rc::new(move |game| {
        let level = game[building_smelter_id]["level"].as_int();
        vec![
            (resource_fire_id, ExpNum::from(2.).pow(level + 10)),
            (resource_earth_id, ExpNum::from(2.).pow(level + 9))
        ]
    }),
    // product
    Rc::new(move |game| {
        let level = game[building_smelter_id]["level"].as_int();
        vec![(resource_metal_id, ExpNum::from(2.).pow(level + 11))]
    }),
    // upgrade cost
    Rc::new(move |game| {
        let level = game[building_smelter_id]["level"].as_int();
        vec![
            (resource_metal_id, ExpNum::from(2.2).pow(level + 8)),
            (resource_man_id, ExpNum::from(1.8).pow(level + 10))
        ]
    }),
    // build cost
    Rc::new(move |_| vec![
        (resource_metal_id, ExpNum::from(2.).pow(10)),
        (resource_fire_id, ExpNum::from(2.).pow(10))
    ]));

    game_def.add_handler("campfire.built", Box::new(move |game, _| {
        if game[building_smelter_id].get("unlocked").map_or(false, |x| x.as_bool()) {
            return;
        }
        game[building_smelter_id].insert("unlocked".to_string(), SValue::Bool(true));
        game.post_building_properties(building_smelter_id);
    }));


    let building_compositor_id = BuildingId(game_def.buildings.len());
    add_building(game_def, Building {
        name: "compositor",
        display_name: json!({
            "en": "Body Compositor",
            "zh": "人体炼成阵",
        }),
        max_level: 100,
        detail: json!({
            "description": {
                "en": r#"Composite Manpower out of thin air."#,
                "zh": r#"炼成人体"#,
            },
            "cost": {
                "en": r#""#,
                "zh": r#""#,
            },
            "product": {
                "en": r#"<ca-katex>\text{{level}}\%</ca-katex> chance: <ca-katex>\text{{Yang}}^{{0.88}}</ca-katex> <ca-resource>man</ca-resource>"#,
                "zh": r#"<ca-katex>\text{{\footnotesize 等级}}\%</ca-katex>几率：<ca-katex>\text{{\footnotesize 阳}}^{{0.88}}</ca-katex><ca-resource>man</ca-resource>"#,
            }
        })
    },
    // cost
    Rc::new(move |_| vec![]),
    // product
    Rc::new(move |game| {
        let level = game[building_compositor_id]["level"].as_int();
        let cutoff = level as f64 / 100.;
        let yang = game[resource_yang_id];
        let _alpha = 0.8791181557867741; // \alpha = \frac{{1}}{{\log_2 2.2}} \approx 0.88

        if game.rand("compositor") <= cutoff {
            vec![(resource_man_id, yang.pow(0.88))]
        } else {
            vec![]
        }
    }),
    // upgrade cost
    Rc::new(move |game| {
        let level = game[building_compositor_id]["level"].as_int();
        vec![(resource_yang_id, ExpNum::from_exp(15) * ExpNum::from(level as f64).pow(10))]
    }),
    // build cost
    Rc::new(move |_| vec![(resource_yang_id, ExpNum::from_exp(15))]));

    game_def.add_handler("nuke.built", Box::new(move |game, _| {
        if game[building_compositor_id].get("unlocked").map_or(false, |x| x.as_bool()) {
            return;
        }
        game[building_compositor_id].insert("unlocked".to_string(), SValue::Bool(true));
        game.post_building_properties(building_compositor_id);
    }));


    let building_compost_id = BuildingId(game_def.buildings.len());
    add_building(game_def, Building {
        name: "compost",
        display_name: json!({
            "en": "Compost Bin",
            "zh": "堆肥箱",
        }),
        max_level: 65535,
        detail: json!({
            "description": {
                "en": r#"A compost bin."#,
                "zh": r#"堆肥箱"#,
            },
            "cost": {
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 15}}</ca-katex> = <ca-building-detail-slot>cost.man</ca-building-detail-slot> <ca-resource>man</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{\tiny 等级}} + 15}}</ca-katex> = <ca-building-detail-slot>cost.man</ca-building-detail-slot><ca-resource>man</ca-resource>"#,
            },
            "product": {
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 15}}</ca-katex> = <ca-building-detail-slot>product.wood</ca-building-detail-slot> <ca-resource>wood</ca-resource><br><ca-katex>2 ^ {{\text{{level}} + 13.5}}</ca-katex> = <ca-building-detail-slot>product.yin</ca-building-detail-slot> <ca-resource>yin</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{\tiny 等级}} + 15}}</ca-katex> = <ca-building-detail-slot>product.wood</ca-building-detail-slot><ca-resource>wood</ca-resource><br><ca-katex>2 ^ {{\text{{\tiny 等级}} + 13.5}}</ca-katex> = <ca-building-detail-slot>product.yin</ca-building-detail-slot><ca-resource>yin</ca-resource>"#,
            }
        })
    },
    // cost
    Rc::new(move |game| {
        let level = game[building_compost_id]["level"].as_int();
        vec![(resource_man_id, ExpNum::from(2.).pow(level + 15))]
    }),
    // product
    Rc::new(move |game| {
        let level = game[building_compost_id]["level"].as_int();
        vec![
            (resource_wood_id, ExpNum::from(2.).pow(level + 15)),
            (resource_yin_id, ExpNum::from(2.).pow(level as f64 + 13.5))
        ]
    }),
    // upgrade cost
    Rc::new(move |game| {
        let level = game[building_compost_id]["level"].as_int();
        vec![(resource_wood_id, ExpNum::from(2.2).pow(level + 15))]
    }),
    // build cost
    Rc::new(move |_| vec![(resource_wood_id, ExpNum::from_exp(18))]));

    game_def.add_handler("compositor.built", Box::new(move |game, _| {
        if game[building_compost_id].get("unlocked").map_or(false, |x| x.as_bool()) {
            return;
        }
        game[building_compost_id].insert("unlocked".to_string(), SValue::Bool(true));
        game.post_building_properties(building_compost_id);
    }));


    let building_volcano_id = BuildingId(game_def.buildings.len());
    add_building(game_def, Building {
        name: "volcano",
        display_name: json!({
            "en": "Volcano",
            "zh": "火山",
        }),
        max_level: 65535,
        detail: json!({
            "description": {
                "en": r#"Active volcano."#,
                "zh": r#"活跃的火山"#,
            },
            "cost": {
                "en": r#""#,
                "zh": r#""#,
            },
            "product": {
                "en": r#"<ca-katex>1\%</ca-katex> chance: <ca-katex>2 ^ {{\text{{level}} + 25}}</ca-katex> <ca-resource>metal</ca-resource><br><ca-katex>1\%</ca-katex> chance: <ca-katex>2 ^ {{\text{{level}} + 25}}</ca-katex> <ca-resource>fire</ca-resource><br><ca-katex>5\%</ca-katex> chance: <ca-katex>2 ^ {{\text{{level}} + 25}}</ca-katex> <ca-resource>earth</ca-resource>"#,
                "zh": r#"<ca-katex>1\%</ca-katex>几率：<ca-katex>2 ^ {{\text{{\tiny 等级}} + 25}}</ca-katex><ca-resource>metal</ca-resource><br><ca-katex>1\%</ca-katex>几率：<ca-katex>2 ^ {{\text{{\tiny 等级}} + 25}}</ca-katex><ca-resource>fire</ca-resource><br><ca-katex>5\%</ca-katex>几率：<ca-katex>2 ^ {{\text{{\tiny 等级}} + 25}}</ca-katex><ca-resource>earth</ca-resource>"#,
            }
        })
    },
    // cost
    Rc::new(move |_game| vec![]),
    // product
    Rc::new(move |game| {
        let level = game[building_volcano_id]["level"].as_int();
        let mut products = vec![];

        if game.rand("volcano.metal") <= 1. / 100. {
            products.push((resource_metal_id, ExpNum::from(2.).pow(level + 25)));
        }

        if game.rand("volcano.fire") <= 1. / 100. {
            products.push((resource_metal_id, ExpNum::from(2.).pow(level + 25)));
        }

        if game.rand("volcano.earth") <= 5. / 100. {
            products.push((resource_metal_id, ExpNum::from(2.).pow(level + 25)));
        }

        products
    }),
    // upgrade cost
    Rc::new(move |game| {
        let level = game[building_volcano_id]["level"].as_int();
        vec![(resource_yang_id, ExpNum::from(2.2).pow(level + 20))]
    }),
    // build cost
    Rc::new(move |_| vec![(resource_yang_id, ExpNum::from_exp(18))]));

    game_def.add_handler("nuke.built", Box::new(move |game, _| {
        if game[building_volcano_id].get("unlocked").map_or(false, |x| x.as_bool()) {
            return;
        }
        game[building_volcano_id].insert("unlocked".to_string(), SValue::Bool(true));
        game.post_building_properties(building_volcano_id);
    }));


    let building_blackaltar_id = BuildingId(game_def.buildings.len());
    add_building(game_def, Building {
        name: "blackaltar",
        display_name: json!({
            "en": "Black Water Altar",
            "zh": "黑水祭坛",
        }),
        max_level: 65536,
        detail: json!({
            "description": {
                "en": r#"A dark altar."#,
                "zh": r#"漆黑的祭坛，有水不断流出。"#,
            },
            "cost": {
                "en": r#""#,
                "zh": r#""#,
            },
            "product": {
                "en": r#"<ca-katex>\frac{{\text{{level}}}}{{\text{{Day}}}} \times \text{{Yin}}</ca-katex> = <ca-building-detail-slot>product.water</ca-building-detail-slot> <ca-resource>water</ca-resource>"#,
                "zh": r#"<ca-katex>\frac{{\text{{\tiny 等级}}}}{{\text{{\tiny 天数}}}} \times \text{{\footnotesize 阴}}</ca-katex> = <ca-building-detail-slot>product.water</ca-building-detail-slot><ca-resource>water</ca-resource>"#,
            }
        })
    },
    // cost
    Rc::new(move |_| vec![]),
    // product
    Rc::new(move |game| {
        let level = game[building_blackaltar_id]["level"].as_int();
        let yin = game[resource_yin_id];
        vec![(resource_water_id, yin * ExpNum::from(level as f64) / ExpNum::from(game.day as f64))]
    }),
    // upgrade cost
    Rc::new(move |game| {
        let level = game[building_blackaltar_id]["level"].as_int();
        vec![(resource_yin_id, ExpNum::from(2.2).pow(level as f64 + 25.))]
    }),
    // build cost
    Rc::new(move |_| vec![(resource_yin_id, ExpNum::from_exp(20))]));

    game_def.add_handler("compost.built", Box::new(move |game, _| {
        if game[building_blackaltar_id].get("unlocked").map_or(false, |x| x.as_bool()) {
            return;
        }
        game[building_blackaltar_id].insert("unlocked".to_string(), SValue::Bool(true));
        game.post_building_properties(building_blackaltar_id);
    }));









}


struct Game {
    history: Vec<JsonValue>, // packed events in the form [day1, event1, arg1, day2, event2, arg2, ...]
    message_queue: Vec<JsonValue>, // accumulated messages to the front in the form [event1, arg1, event2, arg2, ...]

    day: u32,
    resources: Vec<ExpNum>,
    buildings: Vec<BTreeMap<String, SValue>>,
    relics: Vec<BTreeMap<String, SValue>>,
    incomes: Vec<Vec<[SignedExpNum; 10]>>, // [resource][building][ring buffer]

    format_preference: Vec<String>,
    income_preference: u8, // 1 or 10
}

impl Game {
    fn new() -> Game {
        Game {
            history: vec![],
            message_queue: vec![],

            day: 0,
            resources: game_def!().resources.iter().map(|_| ExpNum::from(0.)).collect(),
            buildings: game_def!().buildings.iter().map(|_| BTreeMap::new()).collect(),
            relics: game_def!().relics.iter().map(|_| BTreeMap::new()).collect(),
            incomes: game_def!().resources.iter().map(|_|
                game_def!().buildings.iter().map(|_| [SignedExpNum::positive_zero(); 10]).collect()
            ).collect(),

            format_preference: ["e", "d", "d", "e", "e", "ee"].into_iter().map(|s| s.to_string()).collect(),
            income_preference: 1
        }
    }

    fn rand(&self, salt: &str) -> f64 {
        // TODO: pre-compute the salt if needed
        let bytes: [u8; 4] = unsafe { std::mem::transmute(self.day) };
        let mut state = 0x811c9dc5_u32;
        for b in bytes.into_iter() {
            state = state.wrapping_mul(0x01000193);
            state ^= b as u32;
        }
        for b in salt.bytes() {
            state = state.wrapping_mul(0x01000193);
            state ^= b as u32;
        }
        (state % 1000000) as f64 / 1000000.
    }

    fn dispatch_message(&mut self, event: &str, arg: &JsonValue) -> Option<()> {
        let handler = game_def!().handlers.get(event)?;
        for h in handler {
            h(self, arg);
        }
    
        Some(())
    }

    fn post_message_to_front(&mut self, event: &str, arg: JsonValue) {
        self.message_queue.push(event.into());
        self.message_queue.push(arg);
    }

    fn post_status(&mut self) {
        self.post_message_to_front("status", json!({
            "day": self.day
        }));
    }

    fn post_resources(&mut self) {
        self.post_message_to_front("resources", self.resources.iter().enumerate().map(|(i, r)| {
            (game_def!(ResourceId(i)).name.to_string(), r.format_with_preference(&self.format_preference, "html"))
        }).collect());
    }

    fn post_bug(&mut self, error: &str) {
        self.post_message_to_front("bug", json!({
            "content": error
        }));
    }

    fn post_building_properties(&mut self, building_id: BuildingId) {
        let name = game_def!(building_id).name;
        self.post_message_to_front(&format!("{name}.properties"), self.buildings[building_id.0].iter().map(|(k, v)| {
            (k.to_string(), v.to_string())
        }).collect());
    }

    fn post_relic_properties(&mut self, relic_id: RelicId) {
        let name = game_def!(relic_id).name;
        self.post_message_to_front(&format!("{name}.properties"), self.relics[relic_id.0].iter().map(|(k, v)| {
            (k.to_string(), v.to_string())
        }).collect());
    }

    // max_days is included
    fn fast_forward(&mut self, packed_events: Vec<JsonValue>, max_days: u32, persistent_state: Option<JsonValue>) {
        // load from last checkpoint. i is the index of the last checkpoint day
        let mut i = packed_events.len();
        assert_eq!(i % 3, 0);
        while i > 0 {
            i -= 3;
            if packed_events[i + 1].as_str().unwrap() == "checkpoint" && packed_events[i].as_u64().unwrap() <= max_days as _ {
                break;
            }
        }

        // replay from the last checkpoint, i is the index of day of the event to be dispatched
        while i < packed_events.len() {
            let event_day = packed_events[i].as_u64().unwrap() as u32;
            let event = packed_events[i + 1].as_str().unwrap();
            let arg = &packed_events[i + 2];

            if event_day > max_days {
                while self.day < max_days {
                    self.dispatch_message("step", &JsonNull);
                    self.message_queue.clear();
                }
                break
            }

            if event == "checkpoint" {
                self.day = event_day;
                self.load_state(arg);
                i += 3;
                continue
            }

            while self.day < event_day {
                self.dispatch_message("step", &JsonNull);
                self.message_queue.clear();
            }

            self.dispatch_message(event, arg);
            self.message_queue.clear();
            i += 3;
        }

        self.history = packed_events;
        self.history.truncate(i);

        if i >= 3 && self.history[i - 2].as_str().unwrap() == "dummy" {
            self.history.truncate(i - 3);
        }

        if let Some(persistent_state) = persistent_state {
            self.dispatch_message("persistent_state", &persistent_state);
            self.push_history("persistent_state", persistent_state)
        }
    }

    fn post_everything(&mut self) {
        self.post_status();
        self.post_resources();
        self.post_incomes();
        for (i, _) in game_def!().buildings.iter().enumerate() {
            self.post_building_properties(BuildingId(i));
        }
        for (i, _) in game_def!().relics.iter().enumerate() {
            self.post_relic_properties(RelicId(i));
        }
    }

    // serialize the game state for checkpointing
    fn dump_state(&self) -> JsonValue {
        return json!({
            "resources": self.resources.iter().enumerate().map(|(i, r)| {
                (game_def!(ResourceId(i)).name.to_string(), r.as_exp())
            }).collect::<BTreeMap<String, f64>>(),
            "buildings": self.buildings.iter().enumerate().map(|(i, b)| {
                (game_def!(BuildingId(i)).name.to_string(), b.iter().map(|(k, v)| {
                    (k.to_string(), v.to_string())
                }).collect::<BTreeMap<String, String>>())
            }).collect::<BTreeMap<String, _>>(),
            "relics": self.relics.iter().enumerate().map(|(i, b)| {
                (game_def!(RelicId(i)).name.to_string(), b.iter().map(|(k, v)| {
                    (k.to_string(), v.to_string())
                }).collect::<BTreeMap<String, String>>())
            }).collect::<BTreeMap<String, _>>(),
            "incomes": self.incomes.iter().enumerate().map(|(i, r)| {
                (game_def!(ResourceId(i)).name.to_string(), r.iter().enumerate().map(|(j, b)| {
                    (game_def!(BuildingId(j)).name.to_string(), b.iter().map(|x| SValue::SignedNum(*x).to_string()).collect())
                }).collect())
            }).collect::<BTreeMap<String, BTreeMap<String, JsonValue>>>(),

            "format_preference": self.format_preference.clone(),
            "income_preference": self.income_preference,
        })
    }

    fn load_state(&mut self, state: &JsonValue) {
        for (name, value) in state["resources"].as_object().unwrap().iter() {
            let rid = game_def!().resources.iter().position(|r| r.name == name).unwrap();
            self[ResourceId(rid)] = ExpNum::from_exp(value.as_f64().unwrap_or(f64::NEG_INFINITY));
        }

        for (building_name, properties) in state["buildings"].as_object().unwrap().iter() {
            let bid = game_def!().buildings.iter().position(|b| b.name == building_name).unwrap();
            for (property_name, value) in properties.as_object().unwrap().iter() {
                self[BuildingId(bid)].insert(property_name.to_string(), value.as_str().unwrap().parse().unwrap());
            }
        }

        for (relic_name, properties) in state["relics"].as_object().unwrap().iter() {
            let rid = game_def!().relics.iter().position(|r| r.name == relic_name).unwrap();
            for (property_name, value) in properties.as_object().unwrap().iter() {
                self[RelicId(rid)].insert(property_name.to_string(), value.as_str().unwrap().parse().unwrap());
            }
        }

        for (resource_name, buildings) in state["incomes"].as_object().unwrap().iter() {
            let rid = game_def!().resources.iter().position(|r| r.name == resource_name).unwrap();
            for (building_name, values) in buildings.as_object().unwrap().iter() {
                let bid = game_def!().buildings.iter().position(|b| b.name == building_name).unwrap();
                for (i, value) in values.as_array().unwrap().iter().enumerate() {
                    self.incomes[rid][bid][i] = value.as_str().unwrap().parse::<SValue>().unwrap().as_signed_num();
                }
            }
        }

        self.format_preference = state["format_preference"].as_array().unwrap().iter().map(|x| x.as_str().unwrap().to_string()).collect();
        self.income_preference = state["income_preference"].as_u64().unwrap() as _;
    }

    fn push_history(&mut self, event: impl ToString, arg: JsonValue) {
        self.history.push(self.day.into());
        self.history.push(event.to_string().into());
        self.history.push(arg);
    }

    fn record_income(&mut self, resource: ResourceId, building: BuildingId, amount: SignedExpNum) {
        self.incomes[resource.0][building.0][self.day as usize % 10] += amount;
    }

    fn post_incomes(&mut self) {
        let mut all_incomes = BTreeMap::new();
        for (i, resource) in game_def!().resources.iter().enumerate() {
            let mut resource_incomes = BTreeMap::new();
            let mut accumulated = None;
            for (j, building) in game_def!().buildings.iter().enumerate() {
                let ring_buffer = &self.incomes[i][j];
                let income = match self.income_preference {
                    1 => ring_buffer[self.day as usize % 10],
                    10 => ring_buffer.iter().sum::<SignedExpNum>() / SignedExpNum::from(10.),
                    _ => unreachable!()
                };
                if !income.magnitude.is_zero() {
                    *accumulated.get_or_insert(SignedExpNum::positive_zero()) += income;
                    resource_incomes.insert(building.name.to_string(), income.format_with_preference(&self.format_preference, "html"));
                }
            }
            if let Some(accumulated) = accumulated {
                resource_incomes.insert("accumulated".into(), accumulated.format_with_preference(&self.format_preference, "html"));
            }
            all_incomes.insert(resource.name.to_string(), resource_incomes);
        }
        self.post_message_to_front("incomes", json!(all_incomes));
    }

    // states that persist across time warppings
    fn dump_persistent_state(&mut self) -> JsonValue {
        return json!({
            "forged_relics": self.relics.iter().enumerate()
                .filter(|(_, r)| r.get("forged").map_or(false, |x| x.as_bool()))
                .map(|(i, _)| game_def!(RelicId(i)).name).collect::<Vec<_>>(),
        });
    }

    fn load_persistent_state(&mut self, state: &JsonValue) {
        for relic_name in state["forged_relics"].as_array().unwrap().iter().map(|x| x.as_str().unwrap()) {
            let rid = game_def!().relics.iter().position(|r| r.name == relic_name).unwrap();
            self[RelicId(rid)].insert("unlocked".into(), SValue::Bool(true));
            self[RelicId(rid)].insert("forged".into(), SValue::Bool(true));
        }
    }
}

impl Index<ResourceId> for Game {
    type Output = ExpNum;

    fn index(&self, index: ResourceId) -> &Self::Output {
        &self.resources[index.0]
    }
}

impl IndexMut<ResourceId> for Game {
    fn index_mut(&mut self, index: ResourceId) -> &mut Self::Output {
        &mut self.resources[index.0]
    }
}

impl Index<BuildingId> for Game {
    type Output = BTreeMap<String, SValue>;

    fn index(&self, index: BuildingId) -> &Self::Output {
        &self.buildings[index.0]
    }
}

impl IndexMut<BuildingId> for Game {
    fn index_mut(&mut self, index: BuildingId) -> &mut Self::Output {
        &mut self.buildings[index.0]
    }
}

impl Index<RelicId> for Game {
    type Output = BTreeMap<String, SValue>;

    fn index(&self, index: RelicId) -> &Self::Output {
        &self.relics[index.0]
    }
}

impl IndexMut<RelicId> for Game {
    fn index_mut(&mut self, index: RelicId) -> &mut Self::Output {
        &mut self.relics[index.0]
    }
}


#[no_mangle]
static mut JSON_BUFFER: [u32; 3] = [0, 0, 0];

// write to the json buffer. The client need to call free_json_buffer after reading it.
unsafe fn write_json_buffer(value: &JsonValue) {
    let raw_parts = serde_json::to_vec(value).unwrap().into_raw_parts();
    JSON_BUFFER = [raw_parts.0 as _, raw_parts.1 as _, raw_parts.2 as _];
}

// read from the json buffer AND free it.
unsafe fn read_json_buffer() -> serde_json::Result<JsonValue> {
    let [ptr, len, capacity] = JSON_BUFFER;
    let buffer = Vec::from_raw_parts(ptr as *mut u8, len as _, capacity as _);
    let json = serde_json::from_slice(&buffer);
    json
}

#[no_mangle]
unsafe extern fn alloc_json_buffer(byte_length: u32) {
    let (ptr, len, capacity) = Vec::<u8>::with_capacity(byte_length as _).into_raw_parts();
    JSON_BUFFER = [ptr as _, len as _, capacity as _];
}

#[no_mangle]
unsafe extern fn free_json_buffer() {
    let (ptr, len, capacity) = (JSON_BUFFER[0] as *mut u8, JSON_BUFFER[1] as _, JSON_BUFFER[2] as _);
    let _ = Vec::from_raw_parts(ptr, len, capacity);
}

#[no_mangle]
unsafe extern fn game_new() -> *mut Game {
    if !GAME_DEF.is_initialized() {
        init_game_def()
    }

    Box::into_raw(Box::new(Game::new()))
}

#[no_mangle]
unsafe extern fn game_dump(game: *mut Game) {
    let game = &mut *game;
    game.push_history("dummy", JsonNull); // record the last day
    write_json_buffer(&json!(game.history));
    game.history.truncate(game.history.len() - 3);
}

#[no_mangle]
unsafe extern fn game_load(game: *mut Game) {
    let game = &mut *game;
    assert!(game.history.is_empty());
    if let Ok(mut history) = read_json_buffer() {
        game.fast_forward(std::mem::take(history.as_array_mut().unwrap()), u32::MAX, None);
        game.post_everything();
        write_json_buffer(&std::mem::take(&mut game.message_queue).into());
    } else {
        game.post_bug("failed to parse json");
    }
}

#[no_mangle]
unsafe extern fn game_timewarp(game: *mut Game, day: u32) {
    let game = &mut *game;
    let persistent_state = game.dump_persistent_state();
    game.push_history("dummy", JsonNull);
    let history = std::mem::take(&mut game.history);
    *game = Game::new();
    game.fast_forward(history, day, Some(persistent_state)); // fast_forward automatically trim the dummy event
    game.post_everything();
    write_json_buffer(&std::mem::take(&mut game.message_queue).into());
}

#[no_mangle]
unsafe extern fn game_free(game: *mut Game) {
    let _ = Box::from_raw(game);
}

#[no_mangle]
unsafe extern fn poll(game: *mut Game) {
    let game = &mut *game;

    if let Ok([event, arg]) = read_json_buffer().as_ref().map(|x| &x.as_array().unwrap()[..]) {
        let event = event.as_str().unwrap();
        if event != "step" && !event.ends_with(".detail") {
            game.push_history(event, arg.clone());
        }

        game.dispatch_message(event, &arg);
        game.post_status();
        game.post_resources();
        game.post_incomes();

        if event == "step" && game.day % 65536 == 0 {
            game.push_history("checkpoint", game.dump_state());
        }
    } else {
        game.post_bug("failed to parse json");
    }

    write_json_buffer(&std::mem::take(&mut game.message_queue).into());
}
