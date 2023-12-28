// Day 23: A Long Walk
// We are given a map of a maze; the tiles are # (walls), . (paths), and <>^v
// (directed paths). If you enter a directed path, you have to exit it the way
// it points. We are looking for the longest path through the maze that doesn't
// self-intersect, starting at the top left and exiting at the bottom right.
//
// Looking at the maze, it looks like the paths are quite long and do not branch
// very often, so depth-first search might work, especially if we avoid
// recursing when we only have one out (which will be frequently).

use crate::map2d::{Dir2d,Map2d,Point2d};
use std::collections::HashSet;

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
type PointSet = HashSet<Point2d>;

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

fn neighbors(maze: &Map, p: Point2d, vis: &PointSet, climb: bool) -> Vec<Point2d> {
    let mut r = Vec::new();
    for d in [Dir2d::South, Dir2d::East, Dir2d::West, Dir2d::North] {
        let np = p.moveby(d);
        if !maze.inboundsp(np) || vis.contains(&np) {
            continue;
        }
        let t = *maze.atp(np);
        let okdir = match t {
            Tile::North => d == Dir2d::North || climb,
            Tile::South => d == Dir2d::South || climb,
            Tile::West => d == Dir2d::West || climb,
            Tile::East => d == Dir2d::East || climb,
            Tile::Wall => false,
            _          => true,
        };
        if okdir {
            r.push(np);
        }
    }
    r
}

fn longdfs(maze: &Map, from: Point2d, to: Point2d, visited: &PointSet,
           climb: bool) -> usize {
    let mut from = from;
    let mut visited = visited.clone();
    visited.insert(from);

    let mut outs = neighbors(maze, from, &visited, climb);
    while outs.len() == 1 && from != to {
        from = outs[0];
        visited.insert(from);
        outs = neighbors(maze, from, &visited, climb);
    }

    if from == to {
        visited.len() - 1
    } else {
        outs.iter().map(|p| longdfs(maze, *p, to, &visited, climb)).max().unwrap_or(0)
    }
}

fn dfs(maze: &Map, climb: bool) -> usize {
    let start = maze.topleft().moveby(Dir2d::East);
    let end = maze.bottomright().moveby(Dir2d::West);
    longdfs(maze, start, end, &HashSet::new(), climb)
}

pub fn solve(input: &str) -> (String, String) {
    let maze = parse(input);
    (dfs(&maze, false).to_string(), dfs(&maze, true).to_string())
}
