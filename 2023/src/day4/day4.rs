use std::env;
use std::fs;
use std::str::FromStr;
use std::iter::FromIterator;

use nom::IResult;
use nom::bytes::complete::tag;
use nom::character::complete::{digit1, char};
use nom::combinator::eof;
use nom::multi::{separated_list1, many1};

#[derive(Debug)]
struct Card {
    index : usize,
    winning : Vec<u8>,
    received : Vec<u8>
}

fn main() {
	
    let args: Vec<String> = env::args().collect();
    
    let file_path = &args[1];
         
    println!("In file {}", file_path);

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    println!("With text:\n{}", contents);
    let cards : Vec<Card> = contents.lines().enumerate().try_fold(
        Vec::new(),
        |mut tot, (idx, line)| readCard(idx, line).map(|card| { tot.push(card); tot } )
	).expect("boo");
	
    let score : u32 = cards.iter().fold(0, |mut tot, card| tot + evaluateCard(card));

    let cards2 = cards.iter().map(|x| x).collect();

    let numOfCards = numWonCards(&cards2, &cards2);

    print!("Result part1: {:?}, part2 {:?}", score, numOfCards);
}

fn numWonCards(cardsToEval: &Vec<&Card>, available: &Vec<&Card>) -> usize {
    if(cardsToEval.len() > 0) {
        let wonCards : Vec<&Card>  = cardsToEval.iter().flat_map(|card| cardReward(card, available)).collect();     
        //println!("pass {:?}", cardsToEval.iter().map(|x| x.index).collect::<Vec<usize>>());
        cardsToEval.len() + numWonCards(&wonCards, available)
    } else {
        0
    }
}

fn cardReward<'a>(card : &Card, available: &'a Vec<&Card>) -> Vec<&'a Card> {
    let score = evaluateCardNumWinningNumbers(card) as usize;
    if score > 0 {
        let cardsWonHyp = (card.index + 1)..(card.index + score + 1);
        let cardsWon = cardsWonHyp.filter(|&x| x < available.len()).collect::<Vec<usize>>();
        //println!("Card {} wins {:?}", card.index, cardsWon);
        cardsWon.iter().map(|x| *available.get(*x).unwrap()).collect()
    } else {
        Vec::new()
    }
}


fn evaluateCard(card: &Card) -> u32 {
    let found = evaluateCardNumWinningNumbers(card);
    
    let res = if found > 0 { 2u32.pow(found - 1) } else { 0 };
    //println!("{} {}", found, res);
    res
}

fn evaluateCardNumWinningNumbers(card: &Card) -> u32 {
    card.received.iter().filter(|x| card.winning.contains(&x)).collect::<Vec<&u8>>().len() as u32
}
    


fn readCard(index : usize, input : &str) -> Option<Card> {
    match parseCard(input) {
        Ok((_, (winningStr, receivedStr))) => 
            readNums(winningStr)
                .and_then(|winning| readNums(receivedStr)
                    .map(|received| Card { index: index, winning: winning, received: received })
                ),
        Err(e) => {
            println!("Bad {:?}", e);
            None
            },
    }
}

fn parseCard(input : &str) -> IResult<&str, (Vec<&str>, Vec<&str>)> {
    let (input, _) = tag("Card")(input)?;
    let (input, _) = many1(char(' '))(input)?;
    let (input, cardNr) = digit1(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = many1(char(' '))(input)?;
    let (input, winningStr) = separated_list1(many1(char(' ')), digit1)(input)?;
    let (input, _) = tag(" |")(input)?;
    let (input, _) = many1(char(' '))(input)?;
    let (input, receivedStr) = separated_list1(many1(char(' ')), digit1)(input)?;
    let (input, _) = eof(input)?;
    Ok((input, (winningStr, receivedStr)))
}   

fn readNums(numStrs: Vec<&str>) -> Option<Vec<u8>> {    
    numStrs.iter().map(|x| u8::from_str(x).ok()).collect()
}