// Day 24: Never Tell Me The Odds

use num::abs;

type Point3 = (i64, i64, i64);
type Point3f = (f64, f64, f64);

#[derive(Debug, Eq, PartialEq)]
struct Hailstone {
    p: Point3,
    v: Point3,
}

fn parse_point3(s: &str) -> Point3 {
    let e: Vec<_> = s.split(',').map(|x| x.parse::<i64>().unwrap()).collect();
    (e[0], e[1], e[2])
}

fn parse_hailstone(line: &str) -> Hailstone {
    let stripped = line.replace(" ", "");
    let parts: Vec<_> = stripped.split('@').collect();

    Hailstone { p: parse_point3(parts[0]), v: parse_point3(parts[1]) }
}

fn parse(input: &str) -> Vec<Hailstone> {
    input.split('\n').filter(|x| !x.is_empty()).map(parse_hailstone).collect()
}

fn intersectxy(s1: &Hailstone, s2: &Hailstone) -> Option<(f64, f64)> {
    if s2.v.1 == 0 || s1.v.1 * s2.v.0 == s1.v.0 * s2.v.1 {
        return None;
    }

    let p11 = s1.p.0 as f64;
    let p12 = s2.p.0 as f64;
    let p21 = s1.p.1 as f64;
    let p22 = s2.p.1 as f64;

    let v11 = s1.v.0 as f64;
    let v12 = s2.v.0 as f64;
    let v21 = s1.v.1 as f64;
    let v22 = s2.v.1 as f64;

    let d = (v11 * v22) - (v12 * v21);
    let t1 = ((p21 * v12) - (p22 * v12) + (p12 - p11) * v22) / d;
    let t2 = ((p21 * v11) - (p22 * v11) + (p12 - p11) * v21) / d;

    Some((t1, t2))
}

fn pos(s: &Hailstone, t: f64) -> Point3f {
    (s.p.0 as f64 + s.v.0 as f64 * t,
     s.p.1 as f64 + s.v.1 as f64 * t,
     s.p.2 as f64 + s.v.2 as f64 * t)
}

fn interposxy(s1: &Hailstone, s2: &Hailstone) -> Option<Point3f> {
    if let Some((t1, t2)) = intersectxy(s1, s2) {
        if t1 >= 0.0 && t2 >= 0.0 {
            Some(pos(s1, t1))
        } else {
            None
        }
    } else {
        None
    }
}

fn parta(stones: &Vec<Hailstone>) -> usize {
    const MIN: f64 = 200000000000000.0;
    const MAX: f64 = 400000000000000.0;

    let mut t = 0;
    for s1 in stones {
        for s2 in stones {
            if s1 != s2 {
                if let Some(p) = interposxy(s1, s2) {
                    let inx = p.0 >= MIN && p.0 <= MAX;
                    let iny = p.1 >= MIN && p.1 <= MAX;
                    if inx && iny {
                        t += 1;
                    }
                }
            }
        }
    }
    t / 2
}

fn partb(stones: &Vec<Hailstone>) -> usize {
    print!("eqns : [ ");
    for i in 0 .. 5 {
        print!("pxr + vxr * t_{} = {} + {} * t_{}, ",
                 i, stones[i].p.0, stones[i].v.0, i);
        print!("pyr + vyr * t_{} = {} + {} * t_{}, ",
                 i, stones[i].p.1, stones[i].v.1, i);
        print!("pzr + vzr * t_{} = {} + {} * t_{}, ",
                 i, stones[i].p.2, stones[i].v.2, i);
    }
    println!(" 1 = 1 ];");
    println!("solve(eqns, [pxr, pyr, pzr, vxr, yvr, vzr]);");
    0
}

pub fn solve(input: &str) -> (String, String) {
    let stones = parse(input);
    (parta(&stones).to_string(), partb(&stones).to_string())
}

#[test]
fn test_intersect() {
    let s1 = Hailstone { p: (19, 13, 30), v: (-2, 1, -2) };
    let s2 = Hailstone { p: (18, 19, 22), v: (-1, -1, -2) };

    let inter = intersectxy(&s1, &s2);
    assert!(inter.is_some());
    let (t1, t2) = inter.unwrap();
    let p1 = pos(&s1, t1);
    let p2 = pos(&s2, t2);

    dbg!(t1, t2);
    dbg!(p1, p2);

    assert!(p1.0 > 14.3);
    assert!(p1.0 < 14.4);
}
