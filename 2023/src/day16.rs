// Day 16: The Floor Will Be Lava
// Empty space: . (same direction)
// Mirrors: / \ (reflected 90 degrees depending on the mirror)
// Splitters: | -
// If encountering a splitter parallel to beam, passes through as if empty
// If encountering a splitter perpendicular to beam, reflect 90 degrees both
// ways
// Tile is energized if at least one beam passes through it
// Part A: how many tiles are energized by a beam starting in the top left going
// right?

use std::collections::VecDeque;

use crate::map2d::{Dir2d,Map2d,Point2d};

#[derive(Clone, Copy, Eq, PartialEq)]
enum Tile {
    Space,
    MirWE,
    MirEW,
    SplitNS,
    SplitEW,
}

impl Tile {
    fn step(&self, dir: &Dir2d) -> (Dir2d, Option<Dir2d>) {
        match (*self, *dir) {
            (Tile::Space, d)            => (d,          None),

            // \
            (Tile::MirWE, Dir2d::North)   => (Dir2d::West,  None),
            (Tile::MirWE, Dir2d::South)   => (Dir2d::East,  None),
            (Tile::MirWE, Dir2d::East)    => (Dir2d::South, None),
            (Tile::MirWE, Dir2d::West)    => (Dir2d::North, None),

            // /
            (Tile::MirEW, Dir2d::North)   => (Dir2d::East,  None),
            (Tile::MirEW, Dir2d::South)   => (Dir2d::West,  None),
            (Tile::MirEW, Dir2d::East)    => (Dir2d::North, None),
            (Tile::MirEW, Dir2d::West)    => (Dir2d::South, None),

            (Tile::SplitNS, Dir2d::West)  => (Dir2d::North, Some(Dir2d::South)),
            (Tile::SplitNS, Dir2d::East)  => (Dir2d::North, Some(Dir2d::South)),
            (Tile::SplitNS, d)          => (d,          None),

            (Tile::SplitEW, Dir2d::North) => (Dir2d::West, Some(Dir2d::East)),
            (Tile::SplitEW, Dir2d::South) => (Dir2d::West, Some(Dir2d::East)),
            (Tile::SplitEW, d)          => (d,         None),
        }
    }
}

type Map = Map2d<Tile>;

struct Beam(Point2d, Dir2d);

fn parse(input: &str) -> Map {
    Map2d::from_str(input,
        |cell| match cell {
            '.'  => Tile::Space,
            '\\' => Tile::MirWE,
            '/'  => Tile::MirEW,
            '|'  => Tile::SplitNS,
            '-'  => Tile::SplitEW,
            _    => panic!("bogus tile {}", cell),
        }
    )
}

fn zap(map: &Map, start: Beam) -> usize {
    // TODO: use Map2d for zm and vm
    let mut zm: Map2d<usize> = Map2d::from_size(map.width, map.height, |_| 0);
    let mut vm: Map2d<[bool; 4]> = Map2d::from_size(map.width, map.height,
        |_| [false, false, false, false]);

    let mut zq = VecDeque::new(); 
    zq.push_back(start);

    while !zq.is_empty() {
        let Beam(p, d) = zq.pop_front().unwrap();
        if !map.inboundsp(p) {
            continue;
        }
        if vm.atp(p)[d as usize] {
            continue;
        }
        zm.update(p, |v| *v + 1);
        vm.update(p, |v| {
            let mut a = v.clone();
            a[d as usize] = true;
            a
        });
        let (a, b) = map.atp(p).step(&d);
        zq.push_back(Beam(p.moveby(a), a));
        if let Some(b) = b {
            zq.push_back(Beam(p.moveby(b), b));
        }
    }

    zm.sum(|v| if *v > 0 { 1 } else { 0 })
}

fn parta(map: &Map) -> usize {
    zap(&map, Beam(Point2d { x: 0, y: 0 }, Dir2d::East))
}

fn partb(map: &Map) -> usize {
    let mut best = 0;
    for y in 0 .. map.height {
        let x = 0 as i32;
        let y = y as i32;
        let v = zap(&map, Beam(Point2d { x, y }, Dir2d::East));
        if v > best {
            best = v;
        }

        let x = (map.width - 1) as i32;
        let v = zap(&map, Beam(Point2d { x, y }, Dir2d::West));
        if v > best {
            best = v;
        }
    }

    for x in 0 .. map.width {
        let x = x as i32;
        let y = 0 as i32;
        let v = zap(&map, Beam(Point2d { x, y }, Dir2d::South));
        if v > best {
            best = v;
        }

        let y = (map.height - 1) as i32;
        let v = zap(&map, Beam(Point2d { x, y }, Dir2d::North));
        if v > best {
            best = v;
        }
    }

    best
}

pub fn solve(input: &str) -> (String, String) {
    let map = parse(input);
    (parta(&map).to_string(), partb(&map).to_string())
}
