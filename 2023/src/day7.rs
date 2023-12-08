// Day 7: Camel Cards

use std::cmp::Ordering;

type Card = u8;

#[derive(Clone, Debug, Eq, PartialEq)]
struct Hand {
    cards: Vec<Card>,
    bid: u64,
}

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
enum HandType {
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

fn parse_card(c: char) -> Card {
    match c {
        'A' => 14,
        'K' => 13,
        'Q' => 12,
        'J' => 11,
        'T' => 10,
        '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
            c.to_digit(10).unwrap() as u8,
        '1' => 1,
        _ => panic!("bogus card"),
    }
}

fn parse_hand(line: &str) -> Hand {
    let parts: Vec<_> = line.split(' ').collect();
    let cards: Vec<_> = parts[0].chars().map(parse_card).collect();
    let bid = parts[1].parse::<u64>().unwrap();

    Hand { cards, bid }
}

fn parse(input: &str) -> Vec<Hand> {
    input.split('\n').filter(|s| !s.is_empty()).map(parse_hand).collect()
}

fn type_hand(hand: &Hand) -> HandType {
    let mut counts = [0; 15];
    let mut nj = 0;
    for c in &hand.cards {
        if *c == 1 {
            nj += 1;
        } else {
            counts[*c as usize] += 1;
        }
    }
    counts.sort_by(|a, b| b.cmp(a));
    counts[0] += nj;

    match counts[0..5] {
        [5,0,0,0,0] => HandType::FiveOfAKind,
        [4,1,0,0,0] => HandType::FourOfAKind,
        [3,2,0,0,0] => HandType::FullHouse,
        [3,1,1,0,0] => HandType::ThreeOfAKind,
        [2,2,1,0,0] => HandType::TwoPair,
        [2,1,1,1,0] => HandType::OnePair,
        _           => HandType::HighCard,
    }
}

fn cmp_hands(left: &Hand, right: &Hand) -> Ordering {
    let lt = type_hand(&left);
    let rt = type_hand(&right);

    if lt < rt { return Ordering::Less; }
    if lt > rt { return Ordering::Greater; }

    for i in 0 .. left.cards.len() {
        if left.cards[i] < right.cards[i] { return Ordering::Less; }
        if left.cards[i] > right.cards[i] { return Ordering::Greater; }
    }

    Ordering::Equal
}

fn total_bid(mut hands: Vec<Hand>) -> u64 {
    hands.sort_by(|a, b| cmp_hands(a, b));
    let mut total: u64 = 0;
    for i in 0 .. hands.len() {
        total += hands[i].bid * (i as u64 + 1);
    }
    total
}

fn parta(hands: &Vec<Hand>) -> u64 {
    let hs: Vec<Hand> = hands.clone();
    total_bid(hs)
}

fn partbify(hands: &mut Vec<Hand>) {
    for h in hands {
        for ci in 0 .. h.cards.len() {
            if h.cards[ci] == 11 {
                h.cards[ci] = 1;
            }
        }
    }
}

fn partb(hands: &Vec<Hand>) -> u64 {
    let mut hs: Vec<Hand> = hands.clone();
    partbify(&mut hs);
    total_bid(hs)
}

pub fn solve(input: &str) -> (String, String) {
    let hands = parse(input);
    (parta(&hands).to_string(), partb(&hands).to_string())
}

#[test]
fn test_handtype() {
    assert_eq!(type_hand(&parse_hand("77777 100")), HandType::FiveOfAKind);
    assert_eq!(type_hand(&parse_hand("66665 100")), HandType::FourOfAKind);
    assert_eq!(type_hand(&parse_hand("88555 100")), HandType::FullHouse);

    assert_eq!(type_hand(&parse_hand("32T3K 100")), HandType::OnePair);
    assert_eq!(type_hand(&parse_hand("KK677 100")), HandType::TwoPair);
    assert_eq!(type_hand(&parse_hand("QQQ1A 100")), HandType::FourOfAKind);
    assert_eq!(type_hand(&parse_hand("KT11T 100")), HandType::FourOfAKind);
    assert_eq!(type_hand(&parse_hand("T5515 100")), HandType::FourOfAKind);
}

#[test]
fn test_handcmp() {
    let h1 = parse_hand("77777 100");
    let h2 = parse_hand("55558 100");
    let h3 = parse_hand("44449 100");
    assert_eq!(cmp_hands(&h1, &h2), Ordering::Greater);
    assert_eq!(cmp_hands(&h1, &h3), Ordering::Greater);
    assert_eq!(cmp_hands(&h2, &h3), Ordering::Greater);
    assert_eq!(cmp_hands(&h3, &h1), Ordering::Less);
    assert_eq!(cmp_hands(&h1, &h1), Ordering::Equal);

    let h4 = parse_hand("33332 100");
    let h5 = parse_hand("2AAAA 100");
    assert_eq!(cmp_hands(&h4, &h5), Ordering::Greater);
}
