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

type SolveFn = fn(&str) -> (String, String);

fn main() {
    let args: Vec<String> = env::args().collect();
    let days: Vec<SolveFn> = vec![day0::solve, day1::solve, day2::solve,
                                  day3::solve, day4::solve, day5::solve,
                                  day6::solve, day7::solve];

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
