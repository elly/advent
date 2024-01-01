// Day 0!
// TODO: explain what this is
// part a: given a string of parentheses, compute how deep the nesting goes.
// part b: the same but more cleverly done with a fold

use std::cmp;

fn parta(input: &str) -> i32 {
    let mut depth = 0;
    let mut maxdepth = 0;

    for c in input.chars() {
        if c == '(' {
            depth += 1;
        } else if c == ')' {
            depth -= 1;
        }
        if depth > maxdepth {
            maxdepth = depth;
        }
    }

    maxdepth
}

fn partb(input: &str) -> i32 {
    let (_, md) = input.chars().fold((0, 0), |acc, x| {
        let (d, md) = acc;
        if x == '(' {
            (d + 1, cmp::max(d + 1, md))
        } else if x == ')' {
            (d - 1, md)
        } else {
            (d, md)
        }
    });
    md
}

pub fn solve(input: &str) -> (String, String) {
    (parta(input).to_string(), partb(input).to_string())
}
