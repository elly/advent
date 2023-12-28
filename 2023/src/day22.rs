// Day 22: Sand Slabs
// At first, I did this a very naive way by simulating the falling brick
// physics. That works but is slow and obviously a little silly. I think there
// is a clever way that does not involve a simulation.
//
// If we have, eg:
//   AAA BBB
//    CCCC
//   DD E FF
//  ZZZZZZZZZ
// This can be represented as a directed graph: D and E support C, C supports A
// and B, and Z (the ground) supports D, E, and F. For part A, what we want to
// know is "how many nodes in the graph have no outgoing edges?". For part B,
// what we want to know is basically "which node, if deleted, disconnects the
// most nodes from Z?"
//
// I made a brief attempt at this, but it doesn't work as neatly as I hoped -
// you can have a case like this in the input:
//
//   AAA
//   B
//     C
//   DDD
// Where once the whole thing comes to rest, AAA rests on both B and C, but you
// need to check the actual geometry of where B and C come to rest to know
// whether that's true or not. Hmph :)

use std::collections::HashMap;

type Point3 = (i32, i32, i32);

#[derive(Clone, Debug)]
struct Brick {
    x: (i32, i32),
    y: (i32, i32),
    z: (i32, i32),
    supporting: Vec<usize>,
}

impl Brick {
    fn supports(&self, bm: &HashMap<Point3, usize>) -> Vec<usize> {
        let mut r = Vec::new();

        if self.z.0 == 0 {
            return r;
        }

        for dz in 1..=self.z.0 {
            for x in self.x.0..=self.x.1 {
                for y in self.y.0..=self.y.1 {
                    let b = bm.get(&(x, y, self.z.0 - dz));
                    if let Some(b) = b {
                        if !r.contains(b) {
                            r.push(*b);
                        }
                    }
                }
            }
            if !r.is_empty() {
                return r;
            }
        }
        panic!("???");
    }
}

fn spacemap(bricks: &Vec<Brick>) -> HashMap<Point3, usize> {
    let mut r = HashMap::new();
    for i in 0..bricks.len() {
        let b = &bricks[i];
        for x in b.x.0..=b.x.1 {
            for y in b.y.0..=b.y.1 {
                for z in b.z.0..=b.z.1 {
                    r.insert((x, y, z), i);
                }
            }
        }
    }
    r
}

fn parse_brick(brick: &str) -> Brick {
    let parts: Vec<_> = brick.split('~').collect();
    let mins: Vec<_> = parts[0].split(',').map(|x| x.parse::<i32>().unwrap()).collect();
    let maxs: Vec<_> = parts[1].split(',').map(|x| x.parse::<i32>().unwrap()).collect();

    Brick {
        x: (mins[0], maxs[0]),
        y: (mins[1], maxs[1]),
        z: (mins[2], maxs[2]),
        supporting: Vec::new(),
    }
}

fn parse(input: &str) -> Vec<Brick> {
    let mut bricks: Vec<_> = input.split('\n')
                      .filter(|x| !x.is_empty())
                      .map(parse_brick).collect();
    bricks.push(Brick {
        x: (0, 500),
        y: (0, 500),
        z: (0, 0),
        supporting: Vec::new(),
    });

    let sm = spacemap(&bricks);

    let mut supps = Vec::new();
    for i in 0..bricks.len() {
        supps.push(bricks[i].supports(&sm));
    }

    for i in 0..bricks.len() {
        for s in &supps[i] {
            bricks[*s].supporting.push(i);
        }
    }

    bricks
}

fn parta(world: &Vec<Brick>) -> usize {
    world.iter().filter(|b| b.supporting.is_empty()).count()
}

fn partb(world: &Vec<Brick>) -> usize {
    0
}

pub fn solve(input: &str) -> (String, String) {
    let world = parse(input);
    (parta(&world).to_string(), partb(&world).to_string())
}
