use std::env;
use std::str::Chars;
use std::fs;

fn main() {
	
	let args: Vec<String> = env::args().collect();
    
	let file_path = &args[1];
         
	println!("In file {}", file_path);

	let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

  	println!("With text:\n{}", contents);
	let res : Vec<(u32,u32)> = contents.lines().try_fold(
		Vec::new(),
		|mut tot, line| fistAndLastDigit(&line).map(|x| { tot.push(x); tot } )
	).expect("boo");

	let sum = res.iter().fold(0, |tot : u32, e| tot + e.0 * 10 + e.1);
	
	print!("Result {}", sum)
}

fn fistAndLastDigit(string : &str) -> Option<(u32,u32)> {
	findDigit(&string)
	.and_then(|first| findDigit(&string.chars().rev().collect::<String>().as_str()).map(|second| (first,second)))
}

fn findDigit(string : &str) -> Option<u32> {
	string.chars().find(|x| x.is_ascii_digit()).and_then(|x| x.to_digit(10))
}