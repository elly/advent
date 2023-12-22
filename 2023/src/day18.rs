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
//
// Here's an idea: the resulting shape is the union of a collection of
// rectangles, and if we could figure out where those rectangles are, we could
// compute the total area they should occupy. To do that, we can identify
// outermost edges and find the edges connected to them; once we have three
// connected edges, we can use them to define a rectangle, remove the three
// edges, and splice a new edge in where the rectangle was. Here's an example:
//
//          #######
//          #.....#
//   ########.....#
//   #............#
//   #............221
//   ###............1
//     #............1
//     #............1
//     #####...###221
//         #...#
//         #####
//
// We can pick an outermost edge (marked with 1s) then walk along both its
// perpendicular edges until one of them turns. There is now a rectangle defined
// by those three edges, and we can remove it and add its fourth edge to the
// shape, producing this:
//
//          1111111
//          2.....2
//   #######2.....2
//   #............#
//   #............#
//   ###..........#
//     #..........#
//     #..........#
//     #####...####
//         #...#
//         #####
// And repeat:
//   ##############
//   #............#
//   #............#
//   ###..........#
//     #..........#
//     #..........#
//     #####...####
//         #...#
//         #####
// And repeat continuously, summing the sizes of the rectangles we removed,
// until we run out of rectangles. That seems like it should work.
//
// How do we implement that? It seems like we can keep a list of all the edges
// in the order they appear (which will be easy to produce from the input), then
// gradually remove stuff from that list. We won't actually need many cases, on
// further reflection, since we can always just take the topmost edge and go
// from there...? Does that always work?

use crate::map2d::{Dir2d, Point2d};
use std::cmp;
use std::mem;

type Step = (Dir2d, i32);
type Shape = Vec<Point2d>;

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

fn parse(input: &str, flip: bool) -> Shape {
    let mut p = Point2d { x: 0, y: 0 };
    let mut r = Vec::new();
    // XXX: avoid duplicating the start/end point
    // r.push(p);
    for step in input.split('\n').filter(|x| !x.is_empty()) {
        let (dir, dist) = parse_step(step, flip);
        p = p.movebyn(dir, dist);
        r.push(p);
    }

    r
}

// Returns the index of the leading point (in path order) of the northernmost
// edge in the shape.
fn northmost(shape: &Shape) -> Option<usize> {
    let mut best: Option<usize> = None;

    for i in 0 .. shape.len() {
        if let Some(bp) = best {
            if shape[bp].y > shape[i].y {
                best = Some(i);
            }
        } else {
            best = Some(i);
        }
    }

    best
}

fn validate(s: &Shape) {
    let mut was_v = s[0].x == s[s.len() - 1].x;
    for i in 0 .. s.len() - 2 {
        let is_v = s[i].x == s[i + 1].x;
        let is_h = s[i].y == s[i + 1].y;
        assert!(is_h ^ is_v);
        assert!(was_v == is_h);
        was_v = !was_v;
    }
}

fn carve(s: &Shape) -> (Shape, usize) {
    validate(&s);

    let nm = northmost(s).unwrap();
    assert!(s.len() >= 4);
    // TODO: nasty special case for nm == 0 (like in test input)
    let (mut p0, mut p1, mut p2, mut p3) = (s[nm - 1], s[nm], s[nm + 1], s[nm + 2]);
    let mut pts = Vec::new();

    // If the path is counterclockwise here, flip the order of the points around
    // so they're clockwise:
    if p0.x > p3.x {
        mem::swap(&mut p0, &mut p3);
        mem::swap(&mut p1, &mut p2);
    }

    if p0.y > p3.y {
        // If the lead leg is shorter than the tail leg, we need to leave some
        // of the lead leg behind:
        //     ####
        //     #..#
        //     #..#
        //     #..####
        //     #......
        // becomes:
        //     #######
        //     #......
        pts.push(Point2d { x: p0.x, y: p3.y });
    } else if p0.y < p3.y {
        // And if the lead leg is shorter than the tail leg:
        //         ####
        //     #####..#
        //     #......#
        // becomes:
        //     ########
        //     #......#
        pts.push(Point2d { x: p3.x, y: p0.y });
    } else {
        // Both legs are equal! Just remove all four points.
    }

    dbg!(p0, p1, p2, p3, &pts);

    let dx = (p2.x - p1.x) as usize;
    let dy = (cmp::min(p0.y, p3.y) - p1.y) as usize;

    dbg!(&s);

    let mut c = s.clone();
    let _: Vec<_> = c.splice(nm - 1 .. nm + 3, pts).collect();
    dbg!(&c);
    validate(&c);
    (c, dx * dy)
}

fn recarve(s: &Shape) -> usize {
    let mut s = s.clone();
    let mut t = 0;
    while !s.is_empty() {
        let (ns, dt) = carve(&s);
        s = ns;
        t += dt;
        dbg!(t);
    }
    t
}

pub fn solve(input: &str) -> (String, String) {
    let plana = parse(input, false);
    let planb = parse(input, true);
    dbg!(northmost(&plana));
    (recarve(&plana).to_string(), "".to_string())
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

#[test]
fn test_carve1() {
    let mut s = vec![
        Point2d { x: 6, y: 0 },
        Point2d { x: 6, y: 5 },
        Point2d { x: 4, y: 5 },
        Point2d { x: 4, y: 7 },
        Point2d { x: 6, y: 7 },
        Point2d { x: 6, y: 9 },
        Point2d { x: 1, y: 9 },
        Point2d { x: 1, y: 7 },
        Point2d { x: 0, y: 7 },
        Point2d { x: 0, y: 5 },
        Point2d { x: 2, y: 5 },
        Point2d { x: 2, y: 2 },
        Point2d { x: 0, y: 2 },
        Point2d { x: 0, y: 0 },
    ];
    s.rotate_right(3);
    // Find: (2, 2) - (0, 2), (0, 0), (6, 0), (6, 5) - (4, 5)
    // Need: (2, 2) - (4, 2) - (4, 5)
    //
    //   1######2
    //   #......#
    //   #......#
    //   0###...#
    //      *...3     <--- we need the spliced point to be at *
    //                TODO TODO TODO
    let (c, d) = carve(&s);
}
