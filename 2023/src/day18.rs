// Day 18: Lavaduct Lagoon
//
// Here's the plan: we find the line that encloses the lagoon, then use the
// Shoelace Method to find the area of that polygon. To find the line that
// encloses the lagoon, we walk along the trench which defines its perimeter; we
// need to use the outside edge of that perimeter trench. We don't have an easy
// way to tell which edge is the outside one, so we try walking both edges and
// look for the longest perimeter.
use crate::map2d::{Dir2d,Point2d};
use geo::Area;
use geo::geometry::{Coord,LineString,Polygon};

type Step = (Dir2d, i32);

fn parse_step(input: &str, flip: bool) -> Step {
    let parts: Vec<_> = input.split(' ').collect();
    if flip {
        let s = &parts[2][2 .. parts[2].len() - 1];
        let dist = i32::from_str_radix(
            &s[0 .. s.len() - 1],
            16).unwrap();
        let dir = match &s[s.len() - 1..] {
            "0" => Dir2d::East,
            "1" => Dir2d::South,
            "2" => Dir2d::West,
            "3" => Dir2d::North,
            _   => panic!("bogus dir {}", s),
        };
        (dir, dist)
    } else {
        let dir = match parts[0] {
            "R" => Dir2d::East,
            "D" => Dir2d::South,
            "L" => Dir2d::West,
            "U" => Dir2d::North,
            _   => panic!("bogus dir {}", parts[0]),
        };
        let dist = parts[1].parse::<i32>().unwrap();
        (dir, dist)
    }
}

fn parse(input: &str, flip: bool) -> Vec<Step> {
    input.split('\n').filter(|x| !x.is_empty())
                     .map(|x| parse_step(x, flip))
                     .collect()
}

// We're given the side step entering a corner and the step leaving it; figure
// out whether we need to move this corner outwards in each coordinate or not.
fn corner(p: &Point2d, s0: &Step, s1: &Step, sideb: bool) -> Coord<f64> {
    type C = Coord<f64>;
    let fx = p.x as f64;
    let fy = p.y as f64;
    if sideb {
        match (s0.0, s1.0) {
            (Dir2d::East, Dir2d::South) => C { x: fx + 1., y: fy },
            (Dir2d::East, Dir2d::North) => C { x: fx + 1., y: fy + 1. },
            (Dir2d::West, Dir2d::South) => C { x: fx,      y: fy },
            (Dir2d::West, Dir2d::North) => C { x: fx,      y: fy + 1. },

            (Dir2d::North, Dir2d::East) => C { x: fx + 1., y: fy + 1. },
            (Dir2d::North, Dir2d::West) => C { x: fx + 1., y: fy },
            (Dir2d::South, Dir2d::East) => C { x: fx,      y: fy + 1. },
            (Dir2d::South, Dir2d::West) => C { x: fx,      y: fy },
            _ => panic!("bogus corner"),
        }
    } else {
        match (s0.0, s1.0) {
            (Dir2d::East, Dir2d::South) => C { x: fx + 1., y: fy },
            (Dir2d::East, Dir2d::North) => C { x: fx,      y: fy },
            (Dir2d::West, Dir2d::South) => C { x: fx + 1., y: fy + 1. },
            (Dir2d::West, Dir2d::North) => C { x: fx,      y: fy + 1. },

            (Dir2d::North, Dir2d::East) => C { x: fx,      y: fy },
            (Dir2d::North, Dir2d::West) => C { x: fx,      y: fy + 1. },
            (Dir2d::South, Dir2d::East) => C { x: fx + 1., y: fy },
            (Dir2d::South, Dir2d::West) => C { x: fx + 1., y: fy + 1. },

            _ => panic!("bogus corner"),
        }
    }
}

// This is going from a vector of trench-digging steps to a vector of points
// which lie on one side of that trench; we arbitrarily call the sides side A
// and side B, since we don't know which is which. Each point is formed by the
// intersection of two lines, and we decide whether to outset a given part of
// the coordinate pair (x or y) based on the directions of the two sides.
fn perimeter_side(steps: &Vec<Step>, sideb: bool) -> LineString {
    let mut result = Vec::new();
    let mut p = Point2d { x: 0, y: 0 };

    // The first corner is at (0,0) and its two edges are the last edge and the
    // first edge.
    result.push(corner(&p, &steps[steps.len() - 1], &steps[0], sideb));
    for i in 0 .. steps.len() - 1 {
        p = p.movebyn(steps[i].0, steps[i].1);
        result.push(corner(&p, &steps[i], &steps[i + 1], sideb));
    }

    // Close the loop.
    result.push(result[0]);

    geo::LineString(result)
}

fn perimeter(steps: &Vec<Step>) -> LineString {
    perimeter_side(steps, false)
}

fn parta(input: &str) -> usize {
    let steps = parse(input, false);
    let perim = perimeter(&steps);
    let poly = Polygon::new(perim, Vec::new());
    poly.unsigned_area() as usize
}

fn partb(input: &str) -> usize {
    let steps = parse(input, true);
    let perim = perimeter(&steps);
    let poly = Polygon::new(perim, Vec::new());
    poly.unsigned_area() as usize
}

pub fn solve(input: &str) -> (String, String) {
    (parta(input).to_string(), partb(input).to_string())
}

#[test]
fn test_parse() {
    let (dir, dist) = parse_step("R 6 (#70c710)", false);
    assert_eq!(dir, Dir2d::East);
    assert_eq!(dist, 6);

    let (dir, dist) = parse_step("R 6 (#70c710)", true);
    assert_eq!(dir, Dir2d::East);
    assert_eq!(dist, 461937);
}
