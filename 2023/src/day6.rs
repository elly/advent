// Day 6:

#[derive(Debug)]
struct Race {
    time: u64,
    dist: u64,
}

fn parse(input: &str) -> Vec<Race> {
    let lines: Vec<_> = input.split('\n').collect();
    let times = lines[0].split(' ').filter(|x| *x != "").skip(1);
    let dists = lines[1].split(' ').filter(|x| *x != "").skip(1);
    times.zip(dists).map(|(time, dist)| {
        Race {
            time: time.parse::<u64>().unwrap(),
            dist: dist.parse::<u64>().unwrap()
        }
    }).collect()
}

fn wins(race: &Race, hold: u64) -> bool {
    let dist = hold * (race.time - hold);
    dist > race.dist
}

fn nwins(race: &Race) -> u64 {
    let mut n = 0;
    for i in 0 .. race.time {
        if wins(race, i) {
            n += 1;
        }
    }
    n
}

fn parta(races: &[Race]) -> u64 {
    let mut v = 1;
    for r in races {
        v *= nwins(r);
    }
    v
}

fn flatten(races: &[Race]) -> Race {
    let mut time: u64 = 0;
    let mut dist: u64 = 0;
    let base: u64 = 10;

    for r in races {
        time = time * base.pow(r.time.ilog10() + 1) + r.time;
        dist = dist * base.pow(r.dist.ilog10() + 1) + r.dist;
    }

    Race { time, dist }
}

fn partb(races: &[Race]) -> u64 {
    nwins(&flatten(races))
}

pub fn solve(input: &str) -> (String, String) {
    let races = parse(input);
    (parta(&races).to_string(), partb(&races).to_string())
}
