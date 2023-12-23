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
//
// What about this though:
//
// 1######2
// #......#
// 0#####.*
//      #.#
//      #.3###
// for that we need a new point at *. For this:
//
// 1########2
// #........3##
// 0#######.*..
//        #....
// The big problem is, how do we ensure we're accounting for all the space? For
// a case like the above one, we have to include all the edges (so we don't
// forget them) but not double-count any tiles. It seems like to get that to
// work we'd have to subtract out the area we're leaving behind from the area of
// the rectangle - but that seems like it'd require some case analysis on how
// the rectangle is attached to the larger shape? Yuck.
//
// That whole approach seems like it won't work. Rats.
//
// New approach idea: what if we scanned down the shape line by line? The y
// coordinates probably range across like, 0 - 2**24 or so (say), so what if for
// each y coordinate, we had a notion of which ranges lie inside the shape? The
// left edge of the range is always outside the shape, and crossing a vertical
// line puts you inside it - but what about horizontal lines? are you inside or
// outside afterwards?
//
// We'd also need to keep track of ranges that are inside the shape as we go
// row-by-row, and you're only inside if you are inside for both. So for
// example, let's say we have this shape:
//
//   ####      
//   #  #      ###
//   #  #      # #
//   ## ######## #
//    #    ##    #
//    ###     ####
//      #######
// I think it's not that straightforward. Hm. When we're walking down the walls,
// we lose track of whether we're inside or outside. :(
//
// I went and did some reading and learned about the Shoelace Formula:
// https://en.wikipedia.org/wiki/Shoelace_formula - maybe I can use this, even
// though it's a bit too general. Maybe this will work? I'm worried about how to
// handle the thickness of the lines though - to have a line that encompasses
// the shape, we'd have to outset *some* of the coordinates - the ones that are
// like, on the "inside" of the shape - to account for the extra space on those
// edges.
//
// I'm super stuck on this problem. I'm gonna put it aside for now and come back
// to it.
use crate::map2d::{Dir2d, Point2d};

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

pub fn solve(input: &str) -> (String, String) {
    ("".to_string(), "".to_string())
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
