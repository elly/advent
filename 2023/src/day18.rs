// Day 18: Lavaduct Lagoon
//
// Thinking notes: part B is going to require a fully different approach; the
// lagoon is too big to construct the actual map. We can store the set of
// corners, though, since there's only one corner per instruction. Maybe we
// could then decompose the contained shape into a bunch of separate rectangles
// and add up their sizes? Hmmm.
//
// How would we find a rectangle? Every corner has to be the corner of a
// rectangle, since the whole diagram is a single loop... time to ponder.

use crate::map2d::{Dir2d, Map2d, Point2d};
use std::collections::HashMap;

type Color = u32;
type Step = (Dir2d, usize, Color);
type Plan = Vec<Step>;

#[derive(Clone, Hash)]
struct Tile {
    depth: usize,
    colors: [Color; 4],
}

const TILE_BLANK: Tile = Tile { depth: 0, colors: [0, 0, 0, 0] };

type Map = Map2d<Tile>;

fn parse_step(input: &str) -> Step {
    let parts: Vec<_> = input.split(' ').collect();
    let dir = match parts[0] {
        "R" => Dir2d::East,
        "D" => Dir2d::South,
        "L" => Dir2d::West,
        "U" => Dir2d::North,
        _   => panic!("bogus dir {}", parts[0]),
    };
    let dist = parts[1].parse::<usize>().unwrap();
    let c = u32::from_str_radix(
        &parts[2].replace("(#", "").replace(")", ""),
        16).unwrap();
    (dir, dist, c)
}

fn parse(input: &str) -> Plan {
    input.split('\n').filter(|x| !x.is_empty()).map(parse_step).collect()
}

fn dig(plan: Plan) -> Map {
    let start = Point2d { x: 500, y: 500 };
    let mut p = start;
    let mut d: HashMap<Point2d, Tile> = HashMap::new();
    d.insert(start, Tile { depth: 1, .. TILE_BLANK });
    plan.iter().for_each(|(dir, dist, _color)| {
        // TODO: ignoring colors for the moment
        for _ in 0 .. *dist {
            p = p.moveby(*dir);
            d.insert(p, Tile { depth: 1, .. TILE_BLANK });
        }
    });

    let m = Map::new(1000, 1000, |p| d.get(&p).unwrap_or(&TILE_BLANK).clone());
    let r = m.reachability(Point2d { x: 0, y: 0 }, |p| m.atp(p).depth == 0);
    m.transform(|p| {
        if d.contains_key(&p) { d.get(&p).unwrap().clone() }
        else if !r.atp(p) { Tile { depth: 1, .. TILE_BLANK } }
        else { TILE_BLANK }
    })
}

fn parta(map: &Map) -> usize {
    map.sum(|c| c.depth)
}

pub fn solve(input: &str) -> (String, String) {
    let plan = parse(input);
    let map = dig(plan);
    (parta(&map).to_string(), "".to_string())
}
