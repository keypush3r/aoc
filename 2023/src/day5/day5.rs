use std::env;
use std::fs;
use std::fmt;
use std::str::FromStr;
use std::iter::FromIterator;
use std::collections::HashMap;
use std::ops::Range;
use nom::IResult;
use nom::bytes::complete::tag;
use nom::character::complete::anychar;
use nom::character::complete::line_ending;
use nom::character::complete::none_of;
use nom::character::complete::not_line_ending;
use nom::character::complete::{digit1, char};
use nom::combinator::eof;
use nom::combinator::map_res;
use nom::multi::count;
use nom::multi::{separated_list1, many1};
use ranges::GenericRange;
use ranges::OperationResult;
use std::convert::TryInto;
use core::ops::Bound;
use std::ops::RangeBounds;

#[derive(Debug, Clone)]
struct Mapp {
    destStart : u64,
    sourceStart : u64,
    length: u64,
}

type MRange = Range<u64>;


fn main() {
	
    let args: Vec<String> = env::args().collect();
    
    let file_path = &args[1];
 
    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");
   
    let (_, (seeds,mappings1)) = parse_file(&contents).unwrap();
    let mappings = mappings1.iter().map(|x| x.clone()).take(10).collect();

    let mut part1_res : Vec<(u64, u64)> = seeds.iter().map(|x| (x.to_owned(), find_dest(*x, &mappings).unwrap())).collect();

    part1_res.sort_by(|a,b| a.1.cmp(&b.1));
    println!("Part 1:\n{:?}", part1_res);

    let seedRanges : Vec<MRange> = seeds.chunks(2)
        .map(|x| toRange(x[0],x[1])).take(1000).collect();

    println!("Seed ranges :\n{:?} {:?}", seedRanges, seeds);
    
    if let Some(part2_res) = map_ranges(&seedRanges, &mappings) {
        println!("Part 2:\n{:?}", part2_res);
        part2_res.iter().for_each(|x| println!("Vals {:?}",x.start_bound()))
    }
    
}


// PART 1

fn find_dest(num: u64, mappings: &Vec<(String,Vec<Mapp>)>) -> Option<u64> {
    if let Some((head, tail)) = mappings.split_first() {
        let dest = find_mapp(num, &head.1);
        println!("{} goes to {} at {}", num, dest, mappings.len());
        find_dest(dest,&tail.to_vec()).or(Some(dest))            
    } else {
        None
    }
}

fn find_mapp(num: u64, mappings: &Vec<Mapp>) -> u64 {
    let resOpt = mappings.iter()
        .find(|x| (x.sourceStart..(x.sourceStart + (x.length))).contains(&num));
    if let Some(res) = resOpt {
        source2dest(num, res)
    } else {
        num
    }
}

// PART 2

fn source2dest(num:u64, mapp: &Mapp) -> u64 {
    mapp.destStart + (num - mapp.sourceStart)
}

fn source2dest_bound(numB:Bound<&u64>, mapp: &Mapp) -> u64 {
    let num : u64 = match numB {
        Bound::Included(x) => *x,
        Bound::Excluded(x) => *x,
        Bound::Unbounded => panic!("Can't use unbounded"),
    };
    source2dest(num, mapp)
}

fn map_ranges(seeds: &Vec<MRange>, mappings: &Vec<(String,Vec<Mapp>)>) -> Option<Vec<MRange>> {
    if let Some((head, tail)) = mappings.split_first() {
        let seedsMapped = partition(seeds, &head.1);
        map_ranges(&seedsMapped,&tail.to_vec()).or(Some(seedsMapped))            
    } else {
        None
    }
}

fn partition(seeds:&Vec<MRange>, mapps:&Vec<Mapp>) -> Vec<MRange> {
    let res = seeds.iter().map(|r| partition_range(r.to_owned(), mapps))
        .flat_map(|mut x| { x.0.append(&mut x.1); x.0 }).collect();
    res
}

fn partition_range(inp:MRange, mapps:&Vec<Mapp>) -> (Vec<MRange>, Vec<MRange>) {
    let inpStr = format!("{:?}", inp);
    let res = mapps.iter().fold((vec![], vec![inp]), |mut totA, mapp| {
            let mut res = mapp_ranges(&totA.1, mapp);            
            totA.0.append(&mut res.0);
            (totA.0, res.1)
        }
    );
    res
}
/**
 * return tuple (mapped parts, the new set of unmapped ranges)
 */
fn mapp_ranges(unmapped: &Vec<MRange>, mapp: &Mapp) -> (Vec<MRange>,Vec<MRange>) {
    let (mapped, newUnmapped) = unmapped.iter().fold((vec![], vec![]), |mut tot, e| {
            let mut res = mapped_and_cuts(e.to_owned(), mapp);
            if(res.0.len() == 0) {
                tot.1.push(e.to_owned());
            } else {
                tot.0.append(&mut res.0);            
            }

            if let Some(mut r) = res.1 { 
                tot.1.append(&mut r);
            }
            tot
        }
    );
    (mapped, newUnmapped)
}

fn mapped_and_cuts(inp: MRange, mapp: &Mapp) -> (Vec<MRange>, Option<Vec<MRange>>) {
    let sourceRange = inR(mapp);
    let inpG : GenericRange<u64> = inp.into();
    let (mapped, notMapped) : (Vec<GenericRange<_>>, Option<Vec<GenericRange<_>>>) = match inpG.intersect(sourceRange.into())  {
        OperationResult::Empty => (vec![], None),
        OperationResult::Double(_, _) => panic!("Can't happen"),
        OperationResult::Single(overlap) =>             
            match inpG - overlap {
                OperationResult::Empty => (vec![overlap], None),
                OperationResult::Single(r) => (vec![overlap], Some(vec![r])),
                OperationResult::Double(r1, r2) => (vec![overlap], Some(vec![r1,r2])),                    
            },        
    };

    
    let mappedDest = mapped.iter().map(|x| 
                source2dest_bound(x.start_bound(), mapp)..
                source2dest_bound(x.end_bound(), mapp)
        ).collect();
    let notMapped2 = notMapped.map(|y| 
        y.iter().map(|x| 
            intoRange(*x)).collect()
        );
    (mappedDest, notMapped2)
}

fn intoRange(inp: GenericRange<u64>) -> Range<u64> {
    match (inp.start_bound(), inp.end_bound()) {
        (Bound::Included(x),Bound::Excluded(y)) => (*x..*y),
        _ => panic!("bad range"),
    }
}

fn inR(r:&Mapp) -> MRange {
    toRange(r.sourceStart, r.length)
}

fn toRange(start: u64, length: u64) -> MRange {
    start..(start + length)
}

fn parse_file(input : &str) -> IResult<&str, (Vec<u64>, Vec<(String,Vec<Mapp>)>)> {
    let (input, seeds) = parse_initial_seeds(input)?;
    let (input, _) = count(line_ending,2)(input)?;
    let (input, mappings) = separated_list1(count(line_ending,2), parse_mapping_block)(input)?;
    Ok((input, (seeds, mappings)))
} 

fn parse_mapping_block(input: &str) -> IResult<&str, (String, Vec<Mapp>)> {
    let (input, mapping_name) = many1(none_of(" "))(input)?;
    let (input, _) = tag(" map:\n")(input)?;
    let (input, mappings) = separated_list1(line_ending, parse_mapping)(input)?;
    Ok((input, (mapping_name.iter().collect(), mappings)))
}

fn parse_initial_seeds(input : &str) -> IResult<&str, Vec<u64>> {
    let (input, _) = tag("seeds: ")(input)?;
    separated_list1(char(' '),u64_parser)(input)        
}

fn parse_mapping(input : &str) -> IResult<&str, Mapp> {
    let (input, dest) = u64_parser(input)?;
    let (input, _) = char(' ')(input)?;
    let (input, source) = u64_parser(input)?;
    let (input, _) = char(' ')(input)?;
    let (input, length) = u64_parser(input)?;    
    Ok((input, Mapp { destStart: dest, sourceStart: source, length: length } ))
}   

fn u64_parser(input : &str) -> IResult<&str, u64> {
    map_res(digit1, |x:&str| (x.parse::<u64>()))(input)
}



