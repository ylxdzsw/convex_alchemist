#![allow(clippy::missing_safety_doc)]
#![allow(non_upper_case_globals)]
#![feature(vec_into_raw_parts)]
#![feature(generic_arg_infer)]
#![feature(const_trait_impl)]
#![feature(const_mut_refs)]

use std::{collections::BTreeMap, fmt::Debug, ops::{Index, IndexMut}, rc::Rc};
use indoc::formatdoc;
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

    fn as_exp(&self) -> f64 {
        self.0
    }

    fn pow(self, rhs: impl Into<f64>) -> Self {
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

new_usize_type!(pub, ResourceId);

struct Resource {
    name: &'static str,
    display_name: JsonValue,
}

new_usize_type!(pub, BuildingId);

struct Building {
    name: &'static str,
    display_name: JsonValue,
    detail: JsonValue // in HTML string
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

struct GameDef {
    resources: Vec<Resource>,
    buildings: Vec<Building>,
    handlers: BTreeMap<String, Vec<Box<dyn Fn(&mut Game)>>>
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

impl GameDef {
    const fn new() -> Self {
        Self {
            resources: Vec::new(),
            buildings: Vec::new(),
            handlers: BTreeMap::new()
        }
    }

    fn add_handler(&mut self, name: impl ToString, handler: Box<dyn Fn(&mut Game)>) {
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

    game_def.add_handler("step", Box::new(move |game| {
        game.day += 1
    }));

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


    fn add_building(
        game_def: &mut GameDef,
        building: Building,
        cost_fun: Rc<dyn Fn(&Game) -> Vec<(ResourceId, ExpNum)>>,
        product_fun: Rc<dyn Fn(&Game) -> Vec<(ResourceId, ExpNum)>>,
        upgrade_cost_fun: Rc<dyn Fn(&Game) -> Vec<(ResourceId, ExpNum)>>,
        build_cost_fun: Rc<dyn Fn(&Game) -> Vec<(ResourceId, ExpNum)>>
    ) -> BuildingId {
        let name = building.name;

        let id = BuildingId(game_def.buildings.len());
        game_def.buildings.push(building);

        game_def.add_handler("step".to_string(), Box::new(move |game| {
            if !game[id]["enabled"].as_bool() {
                return;
            }

            let costs = cost_fun(game);

            for &(resource_id, cost) in &costs {
                if game[resource_id] < cost {
                    game[id].insert("enabled".to_string(), BuildingProperty::Bool(false));
                    game.post_update(json!({
                        "event": "building_update",
                        "building": name,
                    }));
                    game.post_update(json!({
                        "event": "log",
                        "msg": {
                            "en": format!("{} is automatically disabled due to lack of resources", name),
                            "zh": format!("{}资源不足，已被自动停用", name)
                        },
                    }));
                    return;
                }
            }

            for (resource_id, cost) in costs {
                game[resource_id] -= cost;
            }

            let products = product_fun(game);
            let mut income_json = serde_json::Map::new(); // due to lifetime issue we have to make it without closure

            for &(resource_id, amount) in &products {
                game[resource_id] += amount;
                income_json.insert(game_def!(resource_id).name.to_string(), json!(amount.as_exp()));
            }

            game.post_update(json!({
                "building": name,
                "income": income_json
            }))
        }));

        game_def.add_handler(format!("{name}.build"), Box::new(move |game| {
            if !game[id]["unlocked"].as_bool() {
                return;
            }

            let costs = build_cost_fun(game);

            for &(resource_id, cost) in &costs {
                if game[resource_id] < cost {
                    game.post_update(json!({
                        "event": "log",
                        "msg": {
                            "en": format!("Not enough {} to build {}", game_def!(resource_id).display_name["en"], name),
                            "zh": format!("{}不足，无法建造{}", game_def!(resource_id).display_name["zh"], name)
                        },
                    }));
                    return;
                }
            }

            for (resource_id, cost) in costs {
                game[resource_id] -= cost;
            }

            game[id].insert("built".to_string(), BuildingProperty::Bool(true));
            game[id].insert("enabled".to_string(), BuildingProperty::Bool(true));
            game[id].insert("level".to_string(), BuildingProperty::Int(1));
            game.post_update(json!({
                "event": "building_update",
                "building": name,
            }));
            game.handle_event(json!({
                "event": format!("{name}.built")
            }));
        }));

        game_def.add_handler(format!("{name}.enable"), Box::new(move |game| {
            if !game[id]["built"].as_bool() {
                return;
            }

            game[id].insert("enabled".to_string(), BuildingProperty::Bool(true));
            game.post_update(json!({
                "event": "building_update",
                "building": name,
            }));
        }));

        game_def.add_handler(format!("{name}.disable"), Box::new(move |game| {
            if !game[id]["built"].as_bool() {
                return;
            }

            game[id].insert("enabled".to_string(), BuildingProperty::Bool(false));
            game.post_update(json!({
                "event": "building_update",
                "building": name,
            }));
        }));

        game_def.add_handler(format!("{name}.upgrade"), Box::new(move |game| {
            if !game[id]["built"].as_bool() {
                return;
            }

            let costs = upgrade_cost_fun(game);

            for &(resource_id, cost) in &costs {
                if game[resource_id] < cost {
                    game.post_update(json!({
                        "event": "log",
                        "msg": {
                            "en": format!("Not enough {} to upgrade {}", game_def!(resource_id).display_name["en"], name),
                            "zh": format!("{}不足，无法升级{}", game_def!(resource_id).display_name["zh"], name)
                        },
                    }));
                    return;
                }
            }

            for (resource_id, cost) in costs {
                game[resource_id] -= cost;
            }

            let current_level = game[id]["level"].as_int();
            game[id].insert("level".to_string(), BuildingProperty::Int(current_level + 1));
            game.post_update(json!({
                "event": "building_update",
                "building": name,
            }));
        }));

        game_def.add_handler(format!("{name}.downgrade"), Box::new(move |game| {
            if !game[id]["built"].as_bool() {
                return;
            }

            let current_level = game[id]["level"].as_int();
            if current_level == 1 {
                return;
            }

            game[id].insert("level".to_string(), BuildingProperty::Int(current_level - 1));
            game.post_update(json!({
                "event": "building_update",
                "building": name,
            }));
        }));

        id
    }


    let building_tent_id = BuildingId(game_def.buildings.len());
    add_building(game_def, Building {
        name: "tent",
        display_name: json!({
            "en": "Tent",
            "zh": "帐篷",
        }),
        detail: json!({
            "en": formatdoc! {r#"
                <p>A tent. Generates man power.</p>
                <p>Products: {man_power} <ca-katex>2 ^ {{\text{{level}} - 1}}</ca-katex> per Day</p>
            "#, man_power = game_def[resource_man_id].display_name["en"] },
            "zh": formatdoc! {r#"
                <p>一个帐篷。生成人力。</p>
                <p>产出: {man_power} <ca-katex>2 ^ {{\text{{等级}} - 1}}</ca-katex> 每天</p>
            "#, man_power = game_def[resource_man_id].display_name["zh"] },
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

    game_def.add_handler("init", Box::new(move |game| {
        game[building_tent_id].insert("unlocked".to_string(), BuildingProperty::Bool(true));
        game[building_tent_id].insert("built".to_string(), BuildingProperty::Bool(true));
        game[building_tent_id].insert("enabled".to_string(), BuildingProperty::Bool(true));
        game[building_tent_id].insert("level".to_string(), BuildingProperty::Int(1));
    }));


}


struct Game {
    rand_state: u32,

    day: u32,

    resources: Vec<ExpNum>,
    buildings: Vec<BTreeMap<String, BuildingProperty>>,

    updates: Vec<JsonValue>,
}

impl Game {
    fn new(rand_state: u32) -> Game {
        Game {
            rand_state,
            day: 5,
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

    fn handle_event(&mut self, message: JsonValue) -> Option<()> {
        let event = message["event"].as_str()?;

        eprintln!("{}", event);

        let handler = game_def!().handlers.get(event)?;
        for h in handler {
            h(self);
        }
        Some(())
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


#[no_mangle]
static mut JSON_BUFFER: [usize; 3] = [0, 0, 0];

// write to the json buffer. The client need to call free_json_buffer after reading it.
unsafe fn write_json_buffer(value: JsonValue) {
    let raw_parts = serde_json::to_vec(&value).unwrap().into_raw_parts();
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
unsafe extern fn game_new(rand_seed: u32) -> *mut Game {
    if !GAME_DEF.is_initialized() {
        init_game_def()
    }

    let mut game = Box::new(Game::new(rand_seed));
    game.handle_event(json!({ "event": "init" }));

    let ptr = Box::into_raw(game);

    write_json_buffer(json!({
        "resources": game_def!().resources.iter().map(|r| json!({
            "name": r.name,
            "display_name": r.display_name,
        })).collect::<Vec<_>>(),

        "buildings": game_def!().buildings.iter().map(|b| json!({
            "name": b.name,
            "display_name": b.display_name,
            "detail": b.detail
        })).collect::<Vec<_>>(),

        "game_resource": (*ptr).resources.iter().enumerate().map(|(i, r)| (game_def!(ResourceId(i)).name.to_string(), r.as_exp())).collect::<BTreeMap<String, f64>>()
    
        ,"game_day": (*ptr).day,
    }));

    ptr
}

#[no_mangle]
unsafe extern fn game_free(game: *mut Game) {
    let _ = Box::from_raw(game);
}

#[no_mangle]
unsafe extern fn handle_event(game: *mut Game) {
    let game = &mut *game;

    if let Ok(event) = read_json_buffer() {
        eprintln!("here");

        game.handle_event(event);
        game.post_update(json!({
            "event": "resources",
            "resources": game.resources.iter().enumerate().map(|(i, r)| (game_def!(ResourceId(i)).name.to_string(), r.as_exp())).collect::<BTreeMap<String, f64>>()
        }));
        game.post_update(json!({
            "event": "status",
            "day": game.day,
        }));

        write_json_buffer(JsonValue::Array(std::mem::take(&mut game.updates)));
    } else {
        write_json_buffer(json!({
            "error": "parsing message failed"
        }));
    }
}

#[no_mangle]
unsafe extern fn dump(game: *mut Game) {
    let game = &mut *game;
    write_json_buffer(json!({
        "resources": game.resources.iter().enumerate().map(|(i, r)| (game_def!(ResourceId(i)).name.to_string(), r.as_exp())).collect::<BTreeMap<String, f64>>(),
        "day": game.day,
    }));
}

#[cfg(test)]
mod test_game {
    use super::*;

    // #[test]
    // fn test_1() {
    //     init_game_def();
    //     let mut game = Game::new(0);
    //     game.handle_event(json!({ "event": "init" }));
    //     eprintln!("{:?}", game.resources);
    //     game.handle_event(json!({ "event": "step" }));
    //     eprintln!("{:?}", game.resources);
    // }

    #[test]
    fn test_2() {
        unsafe {
            let game = game_new(5);
            eprintln!("{:?}", read_json_buffer());
            write_json_buffer(json!({ "event": "step" }));
            handle_event(game);
            eprintln!("{:?}", read_json_buffer());
        }
    }
}
