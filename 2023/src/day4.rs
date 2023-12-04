// Day 4: Scratchcards
// Scratchcards look like: list of winning numbers | list of numbers you have
// First match is 1 point, subsequent matches double the value of the card
// Find the value of the whole pile of scratch cards.

#[derive(Debug)]
struct Card {
    winning: Vec<u32>,
    yours: Vec<u32>,
}

fn parse_numvec(input: &str) -> Vec<u32> {
    input.split(' ')
         .filter(|x| !x.is_empty())
         .map(|x| x.parse::<u32>().expect("number"))
         .collect()
}

fn parse_card(input: &str) -> Card {
    let _ = input.split(':').nth(0).expect("header");
    let rest = input.split(':').nth(1).expect("nums");
    let winning = parse_numvec(rest.split('|').nth(0).expect("winners"));
    let yours = parse_numvec(rest.split('|').nth(1).expect("yours"));
    Card { winning, yours }
}

fn parse(input: &str) -> Vec<Card> {
    input.split('\n').filter(|x| !x.is_empty()).map(parse_card).collect()
}

fn wins(card: &Card) -> u32 {
    let mut nw = 0;
    for y in card.yours.iter() {
        for w in card.winning.iter() {
            if w == y {
                nw += 1
            }
        }
    }
    nw
}

fn value(nw: u32) -> u32 {
    if nw == 0 {
        0
    } else {
        2_u32.pow(nw - 1)
    }
}

fn parta(cards: &[Card]) -> u32 {
    cards.iter().map(|c| value(wins(c))).sum()
}

fn partb(cards: &[Card]) -> u32 {
    // For part b, each card has a number of copies (its multiplier); double the
    // multiplier of a number of cards below it in the list equal to its number
    // of wins.
    let mut counts: Vec<usize> = Vec::new();
    counts.resize(cards.len(), 1);
    for ci in 0 .. cards.len() {
        let w = wins(&cards[ci]) as usize;
        for j in 1 .. w + 1 {
            let i = ci + j;
            if i < counts.len() {
                counts[i] += counts[ci];
            }
        }
    }

    counts.iter().sum::<usize>() as u32
}

pub fn solve(input: &str) -> (String, String) {
    let cards = parse(input);
    (parta(&cards).to_string(), partb(&cards).to_string())
}
