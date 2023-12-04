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
	let res : Vec<Card> = contents.lines().try_fold(
		Vec::new(),
		|mut tot, line| readCard(line).map(|card| { tot.push(card); tot } )
	).expect("boo");
	
    let score : u32 = res.iter().fold(0, |mut tot, card| tot + evaluateCard(card));


	print!("Result {:?}", score);
}

fn evaluateCard(card: &Card) -> u32 {
    let found= card.received.iter().filter(|x| card.winning.contains(&x)).collect::<Vec<&u8>>().len() as u32;
    
    let res = if found > 0 { 2u32.pow(found - 1) } else { 0 };
    println!("{} {}", found, res);
    res
}

fn readCard(input : &str) -> Option<Card> {
    match parseCard(input) {
        Ok((_, (winningStr, receivedStr))) => 
            readNums(winningStr)
                .and_then(|winning| readNums(receivedStr)
                    .map(|received| Card { winning: winning, received: received })
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

