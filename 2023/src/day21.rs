// Day 21: Step Counter

use crate::map2d::{Dir2d,Map2d,Point2d};
use std::collections::{HashSet,VecDeque};

#[derive(Clone, Copy, Eq, PartialEq)]
enum Tile {
    Start,
    Rock,
    Garden,
}

type Map = Map2d<Tile>;

fn parse(input: &str) -> Map {
    Map::from_str(input, |c| {
        match c {
            'S' => Tile::Start,
            '#' => Tile::Rock,
            _   => Tile::Garden,
        }})
}

fn outs(map: &Map, pos: Point2d) -> Vec<Point2d> {
    let mut r = Vec::new();
    for dir in [Dir2d::North, Dir2d::South, Dir2d::East, Dir2d::West] {
        let p = pos.moveby(dir);
        if map.inboundsp(p) && *map.atp(p) != Tile::Rock {
            r.push(p);
        }
    }
    r
}

fn start(map: &Map) -> Point2d {
    for y in 0 .. map.height {
        for x in 0 .. map.width {
            let p = Point2d { x: x as i32, y: y as i32 };
            if *map.atp(p) == Tile::Start {
                return p;
            }
        }
    }
    panic!("no start");
}

fn flood(map: &Map, start: Point2d, max: usize) -> HashSet<Point2d> {
    let mut s = HashSet::new();
    let mut q = VecDeque::new();

    q.push_back((start, 0));

    while !q.is_empty() {
        let (p, d) = q.pop_front().unwrap();
        if d > max || s.contains(&p) {
            continue;
        }

        if d % 2 == 0 {
            s.insert(p);
        }

        for n in outs(map, p) {
            q.push_back((n, d + 1));
        }
    }

    s
}

fn parta(map: &Map) -> usize {
    let flooded = flood(map, start(map), 64);
    flooded.iter().count()
}

pub fn solve(input: &str) -> (String, String) {
    let map = parse(input);
    (parta(&map).to_string(), "".to_string())
}
