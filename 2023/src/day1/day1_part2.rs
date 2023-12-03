use std::env;
use std::fs;
use std::collections::HashMap;
use std::collections::VecDeque;
use nom::IResult;
use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::alpha0;
use nom::character::complete::anychar;
use nom::character::complete::digit1;
use nom::character::complete::satisfy;
use nom::character::complete::{alphanumeric0};
use nom::branch::permutation;
use nom::character::is_alphabetic;
use nom::combinator::eof;
use nom::combinator::not;
use nom::combinator::peek;
use nom::combinator::rest;
use nom::combinator::value;
use nom::error::Error;
use nom::error::ErrorKind;
use nom::multi::many0;
use nom::multi::many_till;
use nom::multi::separated_list0;
use nom::sequence;
use std::convert::TryFrom;
use std::iter::FromIterator;



fn main() {
	
	//assert_eq!(Some(2),findDigit("asdtwoasdf", &makeNumNeedles()));
	
	let res = parser("threeightwo");

	match res {
		Ok(x) => print!("{:?}", x.1),
		Err(_) => print!("err")
	};
	
	let args: Vec<String> = env::args().collect();
    
	let file_path = &args[1];
         
	println!("In file {}", file_path);

	let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

  	println!("With text:\n{}", contents);

	let parsed_nums : Vec<Vec<u32>> = contents.lines().map(|x| to_digits(x)).collect();
	println!("parsed_nums {:?}",parsed_nums);
	println!("parsed_nums size {:?}",parsed_nums.len());
	let res : Vec<(u32,u32)> = parsed_nums.iter().try_fold(
		Vec::new(),
		|mut tot, nums| fistAndLastDigit(nums).map(|x| { tot.push(x); tot } )
	).expect("boo");

	res.iter().for_each(|e| println!("{:?}",e.0*10 + e.1));
	let sum = res.iter().fold(0, |tot : u32, e| tot + e.0 * 10 + e.1);
	
	println!("Result {}", sum)
}

fn to_digits(input: &str) -> Vec<u32> {
	parser(input).unwrap().1
}

fn fistAndLastDigit(nums : &Vec<u32>) -> Option<(u32,u32)> {
	nums.first().and_then(|first| nums.last().map(|second| (first.to_owned(),second.to_owned())))
}

const wordToNum : &[(&str,u32)] = &[
		("one", 1),
		("two", 2),
		("three", 3),
		("four", 4),
		("five", 5),
		("six", 6),
		("seven", 7),
		("eight", 8),
		("nine", 9),
	];

fn parser(input: &str) -> IResult<&str, Vec<u32>> {
	//let (input, _) = many_till(satisfy(is_alphabetic), peek(tokenOrEof))(input)?;
	let (input, _) = many_till(anychar, peek(tokenOrEof))(input)?;
	let (input, res) = many0(makeTokenAndGarbageParser)(input)?;	
	Ok((input,res))
}

fn makeTokenAndGarbageParser(input: &str) -> IResult<&str, u32> {
	let (input, res) = makeTokenParser(input)?;
	let (input, _) = many_till(anychar, peek(tokenOrEof))(input)?;
	Ok((input, res))
}

fn tokenOrEof(input: &str) -> IResult<&str, ()> {
	let mut foo = value((),makeTokenParser).or(value((),eof));
	foo.parse(input)
}

fn makeTokenParser(input: &str) -> IResult<&str, u32> {
	alt((wordDigitParser, digitParser))(input)	
}

fn digitParser(input: &str) -> IResult<&str, u32> {
	let (input, res) = satisfy(|x| x.is_ascii_digit())(input)?;
	match res.to_digit(10) {
		Some(n) => Ok((input, n)),
		None => Err(nom::Err::Failure(Error::new(input, ErrorKind::Tag))),
	}
}

fn wordDigitParser(input: &str) -> IResult<&str, u32> {
	let (input, res) = alt(
			(tag("one"),tag("two"),tag("three"), 
			tag("four"),tag("five"),tag("six"), 
			tag("seven"),tag("eight"),tag("nine"), 
			)
		)(input)?;
	let words : HashMap<String,u32>  = HashMap::from_iter(wordToNum.iter().map(|e| (e.0.to_string(),e.1)));
	match words.get(res)	{
		Some(x) => Ok((input,x.to_owned())),
		None => Err(nom::Err::Failure(Error::new(input, ErrorKind::Tag))),
	}
}