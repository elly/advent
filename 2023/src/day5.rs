// Day 5: If You Give A Seed A Fertilizer
// TODO: brief explanation

#[derive(Debug)]
struct Mapping {
    dest: u64,
    src: u64,
    len: u64,
}

#[derive(Debug)]
struct Input {
    seeds: Vec<u64>,
    maps: Vec<Vec<Mapping>>,
}

fn parse(input: &str) -> Input {
    let mut lines = input.split('\n');
    let seeds: Vec<_> = lines.next().unwrap().split(' ')
                               .skip(1).map(|x| x.parse::<u64>().unwrap())
                               .collect();
    let mut maps: Vec<Vec<Mapping>> = Vec::new();
    while let Some(line) = lines.next() {
        if line.is_empty() {
            maps.push(Vec::<Mapping>::new());
        } else if line.chars().next().unwrap().is_digit(10) {
            let parts: Vec<_> =
                line.split(' ').map(|x| x.parse::<u64>().unwrap()).collect();
            maps.last_mut().unwrap().push(Mapping { 
                dest: parts[0], src: parts[1], len: parts[2]
            });
        }
    }
    Input { seeds, maps }
}

fn convert_one(map: &Vec<Mapping>, seed: u64) -> u64 {
    for m in map {
        if seed >= m.src && seed < m.src + m.len {
            return m.dest + (seed - m.src);
        }
    }
    seed
}

fn convert(maps: &Vec<Vec<Mapping>>, seed: u64) -> u64 {
    maps.iter().fold(seed, |s, m| { convert_one(m, s) })
}

fn parta(input: &Input) -> u64 {
    let mut r: Vec<_> = input.seeds.iter().map(|s| convert(&input.maps, *s)).collect();
    r.sort();
    r[0]
}

// This is really really slow - it takes a few minutes to run on my solution.
// There's probably a more clever way to do this. #450 on global LB though so
// who can say whether it is good or bad
// TODO: this is very slow and naive, do it a different way
fn partb(input: &Input) -> u64 {
    let mut lowest: u64 = 9999999999;
    for r in input.seeds.chunks(2) {
        let start = r[0];
        let len = r[1];
        dbg!((start, len));
        for x in start .. start + len {
            let c = convert(&input.maps, x);
            if c < lowest {
                lowest = dbg!(c);
            }
        }
    }
    lowest
}

pub fn solve(input: &str) -> (String, String) {
    let input = parse(input);
    (parta(&input).to_string(), partb(&input).to_string())
}
