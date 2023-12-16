// Day 15: Lens Library
// Holiday ASCII String Helper algorithm!

fn hash(s: &str) -> u8 {
    let mut v: u8 = 0;
    for c in s.chars() {
        (v, _) = v.overflowing_add(c as u8);
        (v, _) = v.overflowing_mul(17);
    }
    v
}

fn parse(input: &str) -> Vec<String> {
    input.replace("\n", "").split(',').map(String::from).collect()
}

fn parta(input: &Vec<String>) -> usize {
    let mut sum: usize = 0;
    for inst in input.iter() {
        sum += hash(inst) as usize;
    }
    sum
}

#[derive(Debug)]
struct Lens {
    label: String,
    focal: usize,
}

type Boxes = Vec<Vec<Lens>>;

#[derive(Debug, Eq, PartialEq)]
enum Insn {
    Add(String, usize),
    Del(String),
}

impl Insn {
    fn from_str(insn: &str) -> Insn {
        if insn.contains("-") {
            Insn::Del(String::from(&insn[.. insn.len() - 1]))
        } else {
            let mut it = insn.split('=');
            Insn::Add(
                String::from(it.next().unwrap()),
                it.next().unwrap().parse::<usize>().unwrap()
            )
        }
    }

    fn label(&self) -> &str {
        match self {
            Insn::Add(s, _) => &s,
            Insn::Del(s) => &s,
        }
    }
}

fn do_add(b: &mut Vec<Lens>, label: &str, focal: usize) {
    // "immediately behind any lenses already in the box"
    for i in 0 .. b.len() {
        if b[i].label == label {
            b[i].focal = focal;
            return;
        }
    }
    let label = String::from(label);
    b.push(Lens { label, focal });
}

fn do_del(b: &mut Vec<Lens>, label: &str) {
    for i in 0 .. b.len() {
        if b[i].label == label {
            b.remove(i);
            break;
        }
    }
}

fn step(boxes: &mut Boxes, insn: &Insn) {
    let b = hash(insn.label()) as usize;
    match insn {
        Insn::Add(s, f) => do_add(&mut boxes[b], s, *f),
        Insn::Del(s) => do_del(&mut boxes[b], s),
    }
}

fn focalpower(boxes: &Boxes) -> usize {
    let mut total = 0;
    for i in 0 .. boxes.len() {
        let b = &boxes[i];
        for j in 0 .. b.len() {
            total += (i + 1) * (j + 1) * b[j].focal;
        }
    }
    total
}

fn partb(parts: &Vec<String>) -> usize {
    let insns: Vec<_> = parts.iter().map(|x| Insn::from_str(&*x)).collect();
    let mut boxes: Boxes = Vec::new();
    for _ in 0 .. 256 {
        boxes.push(Vec::new());
    }
    insns.iter().for_each(|i| step(&mut boxes, i));
    focalpower(&boxes)
}

pub fn solve(input: &str) -> (String, String) {
    let parts = parse(&input);
    (parta(&parts).to_string(), partb(&parts).to_string())
}

#[test]
fn test_hash() {
    assert_eq!(hash("HASH"), 52);
}

#[test]
fn test_parta() {
    let steps = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";
    assert_eq!(parta(&parse(&steps)), 1320)
}

#[test]
fn test_insnparse() {
    assert_eq!(Insn::Del("foo".to_string()), Insn::from_str("foo-"));
    assert_eq!(Insn::Add("bar".to_string(), 3), Insn::from_str("bar=3"));
}
