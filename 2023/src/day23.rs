// Day 23: A Long Walk
// We are given a map of a maze; the tiles are # (walls), . (paths), and <>^v
// (directed paths). If you enter a directed path, you have to exit it the way
// it points. We are looking for the longest path through the maze that doesn't
// self-intersect, starting at the top left and exiting at the bottom right.
//
// Looking at the maze, it looks like the paths are quite long and do not branch
// very often, so depth-first search might work, especially if we avoid
// recursing when we only have one out (which will be frequently).
//
// TODO: explain how the solution works & why it is fast

use crate::map2d::{Dir2d,Map2d,Point2d};
use std::cmp;

const DIRS: [Dir2d; 4] = [Dir2d::North, Dir2d::South, Dir2d::West, Dir2d::East];

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Tile {
    Path,
    Wall,
    North,
    South,
    East,
    West,
}

type Map = Map2d<Tile>;

fn parse(input: &str) -> Map {
    Map::from_str(input, |c| {
        match c {
            '.' => Tile::Path,
            '#' => Tile::Wall,
            '^' => Tile::North,
            'v' => Tile::South,
            '<' => Tile::West,
            '>' => Tile::East,
            _   => panic!("bogus tile"),
        }
    })
}

#[derive(Debug)]
struct Edge {
    to: usize,
    cost: usize,
}

#[derive(Debug)]
struct Node {
    outs: Vec<Edge>,
}

type Graph = Vec<Node>;

// Builds a graph with weighted edges from the original input map; climb
// indicates whether we should ignore the direction of the directed edges or
// not.
//
// To build the graph, we:
// 1. Identify all the vertexes, which are points on the map with >= 3 adjacent
//    non-wall squares;
// 2. For each vertex, flood outward from it until we reach another vertex in
//    each direction to reconstruct the edges.
fn buildgraph(map: &Map, climb: bool) -> Graph {
    let vs = vertices(map);
    let mut r = Vec::new();

    for vi in 0 .. vs.len() {
        r.push(Node {
            outs: edges(map, &vs, vi, climb),
        });
    }

    r
}

// Returns a Vec of all the vertices in the given map; a vertex is a tile with
// at least 3 routes into it. This ignores whether paths are directed or not. It
// is guaranteed that the first vertex is the entry point and the second vertex
// is the exit point.
fn vertices(map: &Map) -> Vec<Point2d> {
    let mut r = Vec::new();

    // This is a bit of a hack - a more principled solution could look for the
    // hole in the exterior wall to find these instead. Ah well.
    r.push(map.topleft().moveby(Dir2d::East));
    r.push(map.bottomright().moveby(Dir2d::West));

    for y in 1 .. map.height - 1 {
        for x in 1 .. map.width - 1 {
            let p = Point2d { x: x as i32, y: y as i32 };

            if *map.atp(p) == Tile::Wall {
                continue;
            }

            let mut c = 0;
            for d in DIRS {
                let np = p.moveby(d);
                if *map.atp(np) != Tile::Wall {
                    c += 1;
                }
            }

            if c >= 3 {
                r.push(p);
            }
        }
    }
    r
}

// To reconstruct the out edges from the vertex at vi, we walk outwards in each
// direction until we reach another vertex.
fn edges(map: &Map, vs: &Vec<Point2d>, vi: usize, climb: bool) -> Vec<Edge> {
    let mut r = Vec::new();

    fn climbs(cd: Dir2d, t: Tile) -> bool {
        match (cd, t) {
            (Dir2d::North, Tile::South) => true,
            (Dir2d::South, Tile::North) => true,
            (Dir2d::West, Tile::East) => true,
            (Dir2d::East, Tile::West) => true,
            _ => false,
        }
    }

    for dir in DIRS {
        let mut d = dir;
        let mut p = vs[vi].moveby(d);
        let mut dist = 1;

        if !map.inboundsp(p) { continue; }

        let t = *map.atp(p);
        if t == Tile::Wall || (!climb && climbs(d, t)) { continue; }

        while !vs.contains(&p) {
            let mut nd = None;
            for cd in DIRS {
                let np = p.moveby(cd);
                if !map.inboundsp(np) { continue; }
                if cd == d.opposite() { continue; }
                let t = *map.atp(np);
                if t == Tile::Wall { continue; }
                if !climb && climbs(cd, t) { continue; }
                nd = Some(cd);
                break;
            }
            if let Some(nd) = nd {
                d = nd;
                p = p.moveby(d);
                dist += 1;
            } else {
                break;
            }
        }

        if vs.contains(&p) && p != vs[vi] {
            r.push(Edge {
                to: vs.iter().position(|&pp| pp == p).unwrap(),
                cost: dist,
            });
        }
    }

    r
}

// Find the longest path by depth-first search. The visited set is represented
// as a single u64 with one bit per node, corresponding to each node's index in
// the vertex vector. The cost so far is passed down and only returned when we
// actually find the end node.
//
// The original version of this, which operated on the map rather than an
// abstract graph and which therefore needed to use a HashSet for the visited
// intersections, was over 1000x slower (!). Use better data structures, kids!
fn longdfs(g: &Graph, from: usize, to: usize, vs: u64, sofar: usize) -> usize {
    if from == to {
        return sofar;
    }

    let mut best = 0;
    for n in &g[from].outs {
        if vs & (1 << n.to) != 0 { continue; }
        let r = longdfs(g, n.to, to, vs | (1 << from), sofar + n.cost);
        best = cmp::max(best, r);
    }

    best
}

fn longest(map: &Map, climb: bool) -> usize {
    let g = buildgraph(&map, climb);

    longdfs(&g, 0, 1, 1, 0)
}

pub fn solve(input: &str) -> (String, String) {
    let map = parse(input);
    // 6819 too high
    (longest(&map, false).to_string(), longest(&map, true).to_string())
}
