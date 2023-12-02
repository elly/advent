// Day 2: Cube Conundrum
// The elf has a bag of colored cubes. He plays a game where he draws groups of
// colored cubes at random, then shows them to me, so for example "here are 2
// red cubes, 6 green cubes, and 1 blue cube". Games are either possible or
// impossible for any given set of cubes, because they might reveal more cubes
// than were originally in the bag of a given color.

use std::cmp::max;

struct Cubes {
    red: u32,
    green: u32,
    blue: u32,
}

struct Game {
    id: u32,
    draws: Vec<Cubes>,
}

fn parse_draw(input: &str) -> Cubes {
    // 3 blue, 4 red
    let input = input.replace(",", "");
    let mut cubes = Cubes { red: 0, green: 0, blue: 0 };
    let mut last: u32 = 0;
    for t in input.split(' ') {
        if let Ok(v) = t.parse::<u32>() {
            last = v;
        } else if t == "red" {
            cubes.red = last;
        } else if t == "green" {
            cubes.green = last;
        } else if t == "blue" {
            cubes.blue = last;
        }
    }
    cubes
}

fn parse_game(input: &str) -> Game {
    // Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    let parts: Vec<&str> = input.split(':').collect();
    let id = parts[0].split(' ').nth(1).expect("id")
             .parse::<u32>().expect("number");
    let draws = parts[1].split(';').map(parse_draw).collect();

    Game { id, draws }
}

fn parse(input: &str) -> Vec<Game> {
    input.split('\n').filter(|x| !x.is_empty()).map(parse_game).collect()
}

fn is_possible(bag: &Cubes, game: &Game) -> bool {
    game.draws.iter().all(|d| {
        d.red <= bag.red && d.green <= bag.green && d.blue <= bag.blue
    })
}

fn parta(games: &[Game]) -> u32 {
    let bag = Cubes { red: 12, green: 13, blue: 14 };
    games.iter().filter(|g| { is_possible(&bag, g) })
                .map(|g| { g.id }).sum()
}

fn min_cubes(game: &Game) -> Cubes {
    let mut c = Cubes { red: 0, green: 0, blue: 0 };
    for d in game.draws.iter() {
        c.red = max(c.red, d.red);
        c.green = max(c.green, d.green);
        c.blue = max(c.blue, d.blue);
    }
    c
}

fn power(cubes: Cubes) -> u32 {
    cubes.red * cubes.green * cubes.blue
}

fn partb(games: &[Game]) -> u32 {
    games.iter().map(|g| { power(min_cubes(g)) }).sum()
}

pub fn solve(input: &str) -> (String, String) {
    let games = parse(input);
    (String::from(parta(&games[..]).to_string()),
     String::from(partb(&games[..]).to_string()))
}
