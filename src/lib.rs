#![allow(clippy::missing_safety_doc)]
#![allow(non_upper_case_globals)]
#![feature(vec_into_raw_parts)]
#![feature(generic_arg_infer)]
#![feature(const_trait_impl)]

use std::{collections::BTreeMap, fmt::Debug, ops::{Index, IndexMut}};
use serde_json::{json, Value as JsonValue};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
struct ExpNum(f64);

impl ExpNum {
    fn from_exp(f: impl Into<f64>) -> Self {
        let f = f.into();

        if f.is_nan() || (f > 0. && f.is_infinite()) {
            panic!("bad number");
        }

        Self(f)
    }

    fn pow<T: Into<f64>>(self, rhs: T) -> Self {
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

struct Formula {
    calculate: Box<dyn Fn(&Game) -> ExpNum>,
    to_tex: Option<Box<dyn Fn(&str) -> String>>, // the argument is the language
}

impl Debug for Formula {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            match (&GAME_DEF, self.to_tex.as_ref()) {
                (Some(_), Some(to_tex)) => write!(f, "Formula {{{}}}", to_tex("en")),
                _ => write!(f, "Formula"),
            }
        }
    }
}

struct Effect {
    applicable: Box<dyn Fn(&Game) -> bool>,
    apply: Box<dyn Fn(&mut Game)>,
    description: JsonValue,
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
    description: JsonValue,
    status: Box<dyn Fn(&Game) -> BuildingStatus>,
    costs: Vec<(ResourceId, Formula)>,
    products: Vec<(ResourceId, Formula)>,
    operations: Vec<BuildingOperation>,
}

struct BuildingOperation {
    status: Box<dyn Fn(&Game) -> BuildingStatus>,
    costs: Vec<(ResourceId, Formula)>,
    products: Vec<(ResourceId, Formula)>,
    effects: Vec<Effect>,
}

enum BuildingStatus {
    Normal,
    Hide, // haven't unlocked or have already done. Do not show in the UI.
    Unavailable, // not enough resource. Show as gray button.
}

#[derive(Clone, Copy)]
enum BuildingProperty {
    Int(u32),
    Num(ExpNum),
    Bool(bool)
}

impl BuildingProperty {
    fn as_int(&self) -> u32 {
        match self {
            BuildingProperty::Int(x) => *x,
            _ => unreachable!()
        }
    }

    fn as_num(&self) -> ExpNum {
        match self {
            BuildingProperty::Num(x) => *x,
            _ => unreachable!()
        }
    }

    fn as_bool(&self) -> bool {
        match self {
            BuildingProperty::Bool(x) => *x,
            _ => unreachable!()
        }
    }
}

#[derive(Default)]
struct GameDef {
    resources: Vec<Resource>,
    buildings: Vec<Building>,
    event_bus: BTreeMap<String, Vec<Box<dyn Fn(&mut Game)>>>
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


static mut GAME_DEF: Option<GameDef> = None;

macro_rules! game_def {
    () => {
        unsafe { GAME_DEF.as_ref().unwrap_unchecked() }
    }
}

fn init_game_def() -> GameDef {
    let mut game_def = GameDef::default();

    let resource_man_id = ResourceId(game_def.resources.len());
    game_def.resources.push(Resource {
        name: "man",
        display_name: json!({
            "en": "Man Power",
            "zh": "人力",
        })
    });

    let resource_metal_id = ResourceId(game_def.resources.len());
    game_def.resources.push(Resource {
        name: "metal",
        display_name: json!({
            "en": "Metal",
            "zh": "金元素",
        })
    });

    let resource_wood_id = ResourceId(game_def.resources.len());
    game_def.resources.push(Resource {
        name: "wood",
        display_name: json!({
            "en": "Wood",
            "zh": "木元素",
        })
    });

    let resource_water_id = ResourceId(game_def.resources.len());
    game_def.resources.push(Resource {
        name: "water",
        display_name: json!({
            "en": "Water",
            "zh": "水元素",
        })
    });

    let resource_fire_id = ResourceId(game_def.resources.len());
    game_def.resources.push(Resource {
        name: "fire",
        display_name: json!({
            "en": "Fire",
            "zh": "火元素",
        })
    });

    let resource_earth_id = ResourceId(game_def.resources.len());
    game_def.resources.push(Resource {
        name: "earth",
        display_name: json!({
            "en": "Earth",
            "zh": "土元素",
        })
    });

    let resource_yin_id = ResourceId(game_def.resources.len());
    game_def.resources.push(Resource {
        name: "yin",
        display_name: json!({
            "en": "Yin Qi",
            "zh": "阴气",
        })
    });

    let resource_yang_id = ResourceId(game_def.resources.len());
    game_def.resources.push(Resource {
        name: "yang",
        display_name: json!({
            "en": "Yang Qi",
            "zh": "阳气",
        })
    });



    let building_tent_id = BuildingId(game_def.buildings.len());
    game_def.buildings.push(Building {
        name: "tent",
        display_name: json!({
            "en": "Tent",
            "zh": "帐篷",
        }),
        description: json!({
            "en": "A tent. Generates man power.",
            "zh": "一个帐篷。生成人力。",
        }),
        cost: vec![(resource_wood_id, Formula {
                calculate: Box::new(|_| ExpNum::from(1.)),
                to_tex: Box::new(|_, _| "1".to_string())
            })
        ]
    
    // });

    game_def
}


struct Game {
    rand_state: u32,

    resources: Vec<ExpNum>,
    buildings: Vec<BTreeMap<String, BuildingProperty>>,

    updates: Vec<JsonValue>,
}

impl Game {
    fn new(rand_state: u32) -> Game {
        Game {
            rand_state,
            resources: game_def!().resources.iter().map(|_| ExpNum::from(0.)).collect(),
            buildings: game_def!().buildings.iter().map(|_| BTreeMap::new()).collect(),
            updates: vec![]
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

    fn post_update(&mut self, message: JsonValue) {
        self.updates.push(message);
    }

    fn handle_event(&mut self, message: JsonValue) {
        todo!()
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
    type Output = BTreeMap<String, BuildingProperty>;

    fn index(&self, index: BuildingId) -> &Self::Output {
        &self.buildings[index.0]
    }
}

impl IndexMut<BuildingId> for Game {
    fn index_mut(&mut self, index: BuildingId) -> &mut Self::Output {
        &mut self.buildings[index.0]
    }
}


static mut JSON_BUFFER_POINTER: [u32; 3] = [0, 0, 0];

unsafe fn write_json(value: JsonValue) {
    let raw_parts = serde_json::to_vec(&value).unwrap().into_raw_parts();
    JSON_BUFFER_POINTER = [raw_parts.0 as _, raw_parts.1 as _, raw_parts.2 as _];
}

unsafe fn read_json(ptr: *const u8, len: usize) -> serde_json::Result<JsonValue> {
    let slice = std::slice::from_raw_parts(ptr, len);
    serde_json::from_slice(slice)
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
unsafe extern fn free_json_buffer() {
    let (ptr, len, capacity) = (JSON_BUFFER_POINTER[0] as *mut u8, JSON_BUFFER_POINTER[1] as _, JSON_BUFFER_POINTER[2] as _);
    let _ = Vec::from_raw_parts(ptr, len, capacity);
}

#[no_mangle]
unsafe extern fn game_new(rand_seed: u32) -> *mut Game {
    if GAME_DEF.is_none() {
        GAME_DEF = Some(init_game_def());
    }
    Box::into_raw(Box::new(Game::new(rand_seed)))
}

#[no_mangle]
unsafe extern fn game_free(game: *mut Game) {
    let _ = Box::from_raw(game);
}

#[no_mangle]
unsafe extern fn handle_event(game: *mut Game, message_ptr: *const u8, message_len: usize) {
    if let Ok(event) = read_json(message_ptr, message_len) {
        (*game).handle_event(event);
        write_json(JsonValue::Array(std::mem::take(&mut (*game).updates)));
    } else {
        write_json(json!({
            "error": "parsing message failed"
        }));
    }
}
