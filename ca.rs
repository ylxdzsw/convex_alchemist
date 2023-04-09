#![allow(clippy::missing_safety_doc)]
#![allow(non_upper_case_globals)]
#![feature(vec_into_raw_parts)]
#![feature(generic_arg_infer)]
#![feature(const_trait_impl)]
#![feature(const_mut_refs)]

use std::{collections::BTreeMap, fmt::Debug, ops::{Index, IndexMut}, rc::Rc, f64::consts::LN_10, borrow::Borrow};
use serde_json::{json, Value as JsonValue};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
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

    fn to_json(&self) -> JsonValue {
        match self {
            BuildingProperty::Int(x) => json!(["int", x]),
            BuildingProperty::Num(x) => json!(["num", x.as_exp()]),
            BuildingProperty::Bool(x) => json!(["bool", x])
        }
    }

    fn from_json(json: &JsonValue) -> Self {
        match json {
            JsonValue::Array(arr) => {
                match arr[0].as_str().unwrap() {
                    "int" => BuildingProperty::Int(arr[1].as_u64().unwrap() as _),
                    "num" => BuildingProperty::Num(ExpNum::from_exp(arr[1].as_f64().unwrap())),
                    "bool" => BuildingProperty::Bool(arr[1].as_bool().unwrap()),
                    _ => unreachable!()
                }
            },
            _ => unreachable!()
        }
    }
}

struct GameDef {
    resources: Vec<Resource>,
    buildings: Vec<Building>,
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

impl GameDef {
    const fn new() -> Self {
        Self {
            resources: Vec::new(),
            buildings: Vec::new(),
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

    game_def.add_handler("step", Box::new(move |game, _msg| {
        game.day += 1
    }));

    game_def.add_handler("init", Box::new(move |game, msg| {
        game.rand_state = msg["rand_seed"].as_u64().unwrap() as _;

        game.post_message_front(json!({
            "event": "init",
    
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
        }));
    }));

    game_def.add_handler("format_preference.update", Box::new(move |game, msg| {
        game.format_preference = msg["format_preference"].as_array().unwrap().iter().map(|x| x.as_str().unwrap().to_string()).collect();
        game.dispatch_message(json!({
            "event": "format_preference.updated"
        }));
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
        let max_level = building.max_level;

        let id = BuildingId(game_def.buildings.len());
        game_def.buildings.push(building);

        game_def.add_handler("step".to_string(), {
            let cost_fun = cost_fun.clone();
            let product_fun = product_fun.clone();

            Box::new(move |game, _msg| {
                if !game[id].get("enabled").map_or(false, |x| x.as_bool()) {
                    return;
                }

                let costs = cost_fun(game);

                for &(resource_id, cost) in &costs {
                    if game[resource_id] < cost {
                        game[id].insert("enabled".to_string(), BuildingProperty::Bool(false));
                        game.post_building_properties(id);
                        game.post_message_front(json!({
                            "event": "log",
                            "content": {
                                "en": format!("{} is automatically disabled due to lack of resources", game_def!(id).display_name["en"].as_str().unwrap()),
                                "zh": format!("{}资源不足，已自动停用", game_def!(id).display_name["zh"].as_str().unwrap())
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
                    income_json.insert(game_def!(resource_id).name.to_string(), json!(amount.format_with_preference(&game.format_preference, "html")));
                }

                game.post_message_front(json!({
                    "event": format!("{name}.income"),
                    "income": income_json
                }))
            })
        });

        game_def.add_handler(format!("{name}.build"), {
            let build_cost_fun = build_cost_fun.clone();

            Box::new(move |game, _msg| {
                if !game[id].get("unlocked").map_or(false, |x| x.as_bool()) {
                    return;
                }

                let costs = build_cost_fun(game);

                for &(resource_id, cost) in &costs {
                    if game[resource_id] < cost {
                        game.post_message_front(json!({
                            "event": "log",
                            "content": {
                                "en": format!("Not enough {} to build {}", game_def!(resource_id).display_name["en"].as_str().unwrap(), game_def!(id).display_name["en"].as_str().unwrap()),
                                "zh": format!("{}不足，无法建造{}", game_def!(resource_id).display_name["zh"].as_str().unwrap(), game_def!(id).display_name["zh"].as_str().unwrap())
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
                game.post_building_properties(id);
                game.dispatch_message(json!({
                    "event": format!("{name}.built")
                }));
            })
        });

        game_def.add_handler(format!("{name}.enable"), Box::new(move |game, _msg| {
            if !game[id].get("built").map_or(false, |x| x.as_bool()) {
                return;
            }

            game[id].insert("enabled".to_string(), BuildingProperty::Bool(true));
            game.post_building_properties(id);
        }));

        game_def.add_handler(format!("{name}.disable"), Box::new(move |game, _msg| {
            if !game[id].get("built").map_or(false, |x| x.as_bool()) {
                return;
            }

            game[id].insert("enabled".to_string(), BuildingProperty::Bool(false));
            game.post_building_properties(id);
        }));

        game_def.add_handler(format!("{name}.upgrade"), {
            let upgrade_cost_fun = upgrade_cost_fun.clone();

            Box::new(move |game, _msg| {
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
                        game.post_message_front(json!({
                            "event": "log",
                            "content": {
                                "en": format!("Not enough {} to upgrade {}", game_def!(resource_id).display_name["en"].as_str().unwrap(), game_def!(id).display_name["en"].as_str().unwrap()),
                                "zh": format!("{}不足，无法升级{}", game_def!(resource_id).display_name["zh"].as_str().unwrap(), game_def!(id).display_name["zh"].as_str().unwrap())
                            },
                        }));
                        return;
                    }
                }

                for (resource_id, cost) in costs {
                    game[resource_id] -= cost;
                }

                game[id].insert("level".to_string(), BuildingProperty::Int(current_level + 1));
                game.post_building_properties(id);
                game.dispatch_message(json!({
                    "event": format!("{name}.upgraded")
                }));
            })
        });

        game_def.add_handler(format!("{name}.devupgrade"), Box::new(move |game, _msg| {
            let current_level = game[id]["level"].as_int();
            if current_level >= max_level {
                return
            }
            game[id].insert("level".to_string(), BuildingProperty::Int(current_level + 1));
            game.post_building_properties(id);
            game.dispatch_message(json!({
                "event": format!("{name}.upgraded")
            }));
        }));

        game_def.add_handler(format!("{name}.downgrade"), Box::new(move |game, _msg| {
            if !game[id].get("built").map_or(false, |x| x.as_bool()) {
                return;
            }

            let current_level = game[id]["level"].as_int();
            if current_level == 1 {
                return;
            }

            game[id].insert("level".to_string(), BuildingProperty::Int(current_level - 1));
            game.post_building_properties(id);
        }));

        game_def.add_handler(format!("{name}.detail"), {
            let cost_fun = cost_fun.clone();
            let product_fun = product_fun.clone();
            let build_cost_fun = build_cost_fun.clone();
            let upgrade_cost_fun = upgrade_cost_fun.clone();

            Box::new(move |game, _msg| {
                // getting details use some information that is may not always available
                if !game[id].get("unlocked").map_or(false, |x| x.as_bool()) {
                    return;
                }

                let mut message = json!({
                    "event": format!("{name}.detail"),
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
                        message["cost"][name] = json!([amount, sufficient]);
                    }

                    for (resource_id, amount) in product_fun(game) {
                        let name = game_def!(resource_id).name;
                        let amount = amount.format_with_preference(&game.format_preference, "html");
                        message["product"][name] = json!([amount, true]);
                    }

                    if game[id]["level"].as_int() < max_level {
                        for (resource_id, amount) in upgrade_cost_fun(game) {
                            let name = game_def!(resource_id).name;
                            let sufficient = game[resource_id] >= amount;
                            let amount = amount.format_with_preference(&game.format_preference, "html");
                            message["upgrade_cost"][name] = json!([amount, sufficient]);
                        }
                    }
                } else {
                    for (resource_id, amount) in build_cost_fun(game) {
                        let name = game_def!(resource_id).name;
                        let sufficient = game[resource_id] >= amount;
                        let amount = amount.format_with_preference(&game.format_preference, "html");
                        message["build_cost"][name] = json!([amount, sufficient]);
                    }
                }

                game.post_message_front(message);
            })
        });

        id
    }


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

    game_def.add_handler("init", Box::new(move |game, _msg| {
        game[building_tent_id].insert("unlocked".to_string(), BuildingProperty::Bool(true));
        game[building_tent_id].insert("built".to_string(), BuildingProperty::Bool(true));
        game[building_tent_id].insert("enabled".to_string(), BuildingProperty::Bool(true));
        game[building_tent_id].insert("level".to_string(), BuildingProperty::Int(1));
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
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 10}} - \text{{MP}} ^ {{0.8}}</ca-katex> = <ca-building-detail-slot>product.wood</ca-building-detail-slot> <ca-resource>wood</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{\tiny 等级}} + 10}} - \text{{人}} ^ {{0.8}}</ca-katex> = <ca-building-detail-slot>product.wood</ca-building-detail-slot> <ca-resource>wood</ca-resource>"#,
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

    game_def.add_handler("tent.upgraded", Box::new(move |game, _msg| {
        if game[building_forest_id].get("unlocked").map_or(false, |x| x.as_bool()) {
            return;
        }
        let tent_level = game[building_tent_id]["level"].as_int();
        if tent_level >= 5 {
            game[building_forest_id].insert("unlocked".to_string(), BuildingProperty::Bool(true));
            game[building_forest_id].insert("built".to_string(), BuildingProperty::Bool(true));
            game[building_forest_id].insert("enabled".to_string(), BuildingProperty::Bool(true));
            game[building_forest_id].insert("level".to_string(), BuildingProperty::Int(1));
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
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 8}} \times \frac{{\text{{Water}} + 1}}{{\text{{Water}} + \text{{Earth}} + 1}}</ca-katex> = <ca-building-detail-slot>product.water</ca-building-detail-slot> <ca-resource>water</ca-resource><br><ca-katex>2 ^ {{\text{{level}} + 8}} \times \frac{{\text{{Earth}} + 1}}{{\text{{Water}} + \text{{Earth}} + 1}}</ca-katex> = <ca-building-detail-slot>product.earth</ca-building-detail-slot> <ca-resource>earth</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{level}} + 8}} \times \frac{{\text{{水}} + 1}}{{\text{{水}} + \text{{土}} + 1}}</ca-katex> = <ca-building-detail-slot>product.water</ca-building-detail-slot> <ca-resource>water</ca-resource><br><ca-katex>2 ^ {{\text{{level}} + 8}} \times \frac{{\text{{土}} + 1}}{{\text{{水}} + \text{{土}} + 1}}</ca-katex> = <ca-building-detail-slot>product.earth</ca-building-detail-slot> <ca-resource>earth</ca-resource>"#,
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
            (resource_water_id, ExpNum::from(2.3).pow(level+10)),
            (resource_earth_id, ExpNum::from(2.3).pow(level+10)),
        ]
    }),
    // build cost
    Rc::new(move |_| vec![]));

    game_def.add_handler("tent.upgraded", Box::new(move |game, _msg| {
        if game[building_swamp_id].get("unlocked").map_or(false, |x| x.as_bool()) {
            return;
        }
        let tent_level = game[building_tent_id]["level"].as_int();
        if tent_level >= 5 {
            game[building_swamp_id].insert("unlocked".to_string(), BuildingProperty::Bool(true));
            game[building_swamp_id].insert("built".to_string(), BuildingProperty::Bool(true));
            game[building_swamp_id].insert("enabled".to_string(), BuildingProperty::Bool(true));
            game[building_swamp_id].insert("level".to_string(), BuildingProperty::Int(1));
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
        vec![(resource_man_id, ExpNum::from(2.).pow(level+10))]
    }),
    // build cost
    Rc::new(move |_| vec![(resource_man_id, ExpNum::from(10000.))]));

    game_def.add_handler("tent.upgraded", Box::new(move |game, _msg| {
        if game[building_campfire_id].get("unlocked").map_or(false, |x| x.as_bool()) {
            return;
        }
        let tent_level = game[building_tent_id]["level"].as_int();
        if tent_level >= 5 {
            game[building_campfire_id].insert("unlocked".to_string(), BuildingProperty::Bool(true));
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
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 8.5}}</ca-katex> = <ca-building-detail-slot>cost.man</ca-building-detail-slot> <ca-resource>man</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{\tiny 等级}} + 8.5}}</ca-katex> = <ca-building-detail-slot>cost.man</ca-building-detail-slot> <ca-resource>man</ca-resource>"#,
            },
            "product": {
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 6}}</ca-katex> = <ca-building-detail-slot>product.metal</ca-building-detail-slot> <ca-resource>metal</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{\tiny 等级}} + 6}}</ca-katex> = <ca-building-detail-slot>product.metal</ca-building-detail-slot> <ca-resource>metal</ca-resource>"#,
            }
        })
    },
    // cost
    Rc::new(move |game| {
        let level = game[building_mine_id]["level"].as_int() as f64;
        vec![(resource_man_id, ExpNum::from(2.).pow(level + 8.5))]
    }),
    // product
    Rc::new(move |game| {
        let level = game[building_mine_id]["level"].as_int();
        vec![(resource_metal_id, ExpNum::from(2.).pow(level + 6))]
    }),
    // upgrade cost
    Rc::new(move |game| {
        let level = game[building_mine_id]["level"].as_int();
        vec![(resource_man_id, ExpNum::from(2.).pow(level + 10))]
    }),
    // build cost
    Rc::new(move |_| vec![(resource_man_id, ExpNum::from(10000.))]));

    game_def.add_handler("tent.upgraded", Box::new(move |game, _msg| {
        if game[building_mine_id].get("unlocked").map_or(false, |x| x.as_bool()) {
            return;
        }
        let tent_level = game[building_tent_id]["level"].as_int();
        if tent_level >= 5 {
            game[building_mine_id].insert("unlocked".to_string(), BuildingProperty::Bool(true));
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
                "en": r#"<ca-katex>2 ^ {{\text{{level}} + 6}}</ca-katex> = <ca-building-detail-slot>product.man</ca-building-detail-slot> <ca-resource>man</ca-resource>"#,
                "zh": r#"<ca-katex>2 ^ {{\text{{\tiny 等级}} + 6}}</ca-katex> = <ca-building-detail-slot>product.man</ca-building-detail-slot> <ca-resource>man</ca-resource>"#,
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
        vec![(resource_man_id, ExpNum::from(2.).pow(level + 6))]
    }),
    // upgrade cost
    Rc::new(move |game| {
        let level = game[building_farm_id]["level"].as_int();
        vec![(resource_man_id, ExpNum::from(2.).pow(level + 10))]
    }),
    // build cost
    Rc::new(move |_| vec![
        (resource_man_id, ExpNum::from(1000.)),
        (resource_water_id, ExpNum::from(1000.))
    ]));

    game_def.add_handler("tent.upgraded", Box::new(move |game, _msg| {
        if game[building_farm_id].get("unlocked").map_or(false, |x| x.as_bool()) {
            return;
        }
        let tent_level = game[building_tent_id]["level"].as_int();
        if tent_level >= 10 {
            game[building_farm_id].insert("unlocked".to_string(), BuildingProperty::Bool(true));
            game.post_building_properties(building_farm_id);
        }
    }));

}


struct Game {
    rand_state: u32,
    message_queue: Vec<JsonValue>,

    history: Vec<JsonValue>,

    day: u32,
    resources: Vec<ExpNum>,
    buildings: Vec<BTreeMap<String, BuildingProperty>>,

    format_preference: Vec<String>
}

impl Game {
    fn new() -> Game {
        Game {
            rand_state: 393939,
            message_queue: vec![],

            history: vec![],

            day: 0,
            resources: game_def!().resources.iter().map(|_| ExpNum::from(0.)).collect(),
            buildings: game_def!().buildings.iter().map(|_| BTreeMap::new()).collect(),

            format_preference: ["e", "d", "d", "e", "e", "ee"].into_iter().map(|s| s.to_string()).collect()
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

    fn post_message_front(&mut self, message: JsonValue) {
        self.message_queue.push(message);
    }

    fn dispatch_message(&mut self, message: JsonValue) -> Option<()> {
        let event = message["event"].as_str()?;

        let handler = game_def!().handlers.get(event)?;
        for h in handler {
            h(self, &message);
        }
        Some(())
    }

    fn post_status(&mut self) {
        self.post_message_front(json!({
            "event": "status",
            "day": self.day
        }));
    }

    fn post_resources(&mut self) {
        self.post_message_front(json!({
            "event": "resources",
            "resources": self.resources.iter().enumerate().map(|(i, r)| {
                (game_def!(ResourceId(i)).name.to_string(), r.format_with_preference(&self.format_preference, "html"))
            }).collect::<BTreeMap<String, String>>()
        }));
    }

    fn post_bug(&mut self, error: &str) {
        self.post_message_front(json!({
            "event": "bug",
            "content": error
        }));
    }

    fn post_building_properties(&mut self, building_id: BuildingId) {
        let name = game_def!(BuildingId(building_id.0)).name;
        self.post_message_front(json!({
            "event": format!("{name}.properties"),
            "properties": self.buildings[building_id.0].iter().map(|(k, v)| {
                (k.to_string(), v.to_json())
            }).collect::<BTreeMap<String, JsonValue>>()
        }));
    }

    fn fast_forward(&mut self, events: Vec<JsonValue>, max_days: u32) {
        for event in events {
            self.history.push(event.clone());
            self.dispatch_message(event);
            self.message_queue.clear();
            if self.day >= max_days {
                break;
            }
        }
    }

    fn post_everything(&mut self) {
        self.post_status();
        self.post_resources();
        for (i, _) in game_def!().buildings.iter().enumerate() {
            self.post_building_properties(BuildingId(i));
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
static mut JSON_BUFFER: [u32; 3] = [0, 0, 0];

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
unsafe extern fn game_new() -> *mut Game {
    if !GAME_DEF.is_initialized() {
        init_game_def()
    }

    Box::into_raw(Box::new(Game::new()))
}

#[no_mangle]
unsafe extern fn game_dump(game: *mut Game) {
    let game = &mut *game;
    write_json_buffer(json!(game.history))
}

#[no_mangle]
unsafe extern fn game_load(game: *mut Game) {
    let game = &mut *game;
    assert!(game.history.is_empty());
    if let Ok(history) = read_json_buffer() {
        game.fast_forward(history.as_array().unwrap().clone(), u32::MAX);
        game.post_everything();
        write_json_buffer(json!(std::mem::take(&mut game.message_queue)));
    } else {
        game.post_bug("failed to parse json");
    }
}

#[no_mangle]
unsafe extern fn game_timewarp(game: *mut Game, day: u32) {
    let game = &mut *game;
    let history = std::mem::take(&mut game.history);
    *game = Game::new();
    game.fast_forward(history, day);
    game.post_everything();
    write_json_buffer(json!(std::mem::take(&mut game.message_queue)));
}

#[no_mangle]
unsafe extern fn game_free(game: *mut Game) {
    let _ = Box::from_raw(game);
}

#[no_mangle]
unsafe extern fn poll(game: *mut Game) {
    let game = &mut *game;

    if let Ok(event) = read_json_buffer() {
        if !event["event"].as_str().unwrap().ends_with(".detail") { // TODO
            game.history.push(event.clone());
        }

        game.dispatch_message(event);
        game.post_status();
        game.post_resources();
    } else {
        game.post_bug("failed to parse json");
    }

    write_json_buffer(json!(std::mem::take(&mut game.message_queue)));
}

#[cfg(test)]
mod test_game {
    use super::*;

    #[test]
    fn test_1() {
        init_game_def();
        let mut game = Game::new();
        game.dispatch_message(json!({ "event": "init", "rand_seed": 393939 }));
        eprintln!("{:?}", game.resources);
        game.dispatch_message(json!({ "event": "step" }));
        eprintln!("{:?}", game.resources);
    }
}
