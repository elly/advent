use std::env;
use std::io::{self, Read};

mod day0;
mod day1;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;
mod day18;
mod day19;
mod day20;

mod map2d;

type SolveFn = fn(&str) -> (String, String);

fn main() {
    let args: Vec<String> = env::args().collect();
    let days: Vec<SolveFn> = vec![day0::solve,  day1::solve,  day2::solve,
                                  day3::solve,  day4::solve,  day5::solve,
                                  day6::solve,  day7::solve,  day8::solve,
                                  day9::solve,  day10::solve, day11::solve,
                                  day12::solve, day13::solve, day14::solve,
                                  day15::solve, day16::solve, day17::solve,
                                  day18::solve, day19::solve, day20::solve];

    if args.len() < 2 {
        panic!("Usage: <day number>");
    }
    let dn = args[1].parse::<usize>().unwrap();
    if dn >= days.len() {
        panic!("Bogus day number");
    }

    let mut input = String::new();
    io::stdin().read_to_string(&mut input).expect("input?");

    let (outa, outb) = days[dn](&input);

    println!("{} {}", outa, outb);
}
