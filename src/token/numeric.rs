use super::token::DynParser;
use super::token::Token;
use rusty_parser as rp;

use rp::IntoParser;

/*
D			[0-9]
H     [0-9a-fA-F]
IS		(u|U|l|L)*

0[xX]{H}+{IS}?		{ count(); return(CONSTANT); }
0{D}+{IS}?		{ count(); return(CONSTANT); }
{D}+{IS}?		{ count(); return(CONSTANT); }
*/
pub fn integer_numeric() -> DynParser {
    let digit = ('0'..='9').into_parser();
    let hex_low = ('a'..='f').into_parser();
    let hex_up = ('A'..='F').into_parser();

    let dec = digit;
    let oct = ('0'..='7').into_parser();
    let hex = rp::or!(digit, hex_low, hex_up);

    let dec_int = rp::seq('1'..='9', dec.repeat(0..)).void();

    let oct_int = rp::seq('0', oct.repeat(0..)).void();

    let hex_int = rp::seq!("0x", hex.repeat(1..)).void();

    let integer_numeric = rp::or!(hex_int, oct_int, dec_int);

    let suffix = rp::or!('u', 'U', 'l', 'L');

    let integer_numeric = rp::seq!(integer_numeric.string(), suffix.optional());

    DynParser::new(integer_numeric.map(|s: String, suffix: Option<char>| {
        let parse_res = s.parse::<u64>().expect("Failed to parse String to u64");
        let suffix = suffix.or(Some('u')).unwrap();
        match suffix {
            'u' | 'U' => Token::ConstantInteger(parse_res as u32),
            'l' | 'L' => Token::ConstantLong(parse_res),
            _ => panic!("Invalid suffix for integer"),
        }
    }))
}

/*
D			[0-9]
E			[Ee][+-]?{D}+
FS		(f|F|l|L)

{D}+{E}{FS}?		{ count(); return(CONSTANT); }
{D}*"."{D}+({E})?{FS}?	{ count(); return(CONSTANT); }
{D}+"."{D}*({E})?{FS}?	{ count(); return(CONSTANT); }
*/

pub fn float_numeric() -> DynParser {
    let digit = ('0'..='9').into_parser();

    let exp = rp::seq!(
        rp::or!('e', 'E'),
        rp::or!('+', '-').optional(),
        digit.repeat(1..)
    );

    let suffix = rp::or!('f', 'F', 'l', 'L').optional();

    let case1 = rp::seq!(digit.repeat(1..), exp);
    let case2 = rp::seq!(digit.repeat(0..), '.', digit.repeat(1..), exp.optional());
    let case3 = rp::seq!(digit.repeat(1..), '.', digit.repeat(0..), exp.optional());

    let float_numeric = rp::or!(case1.void(), case2.void(), case3.void());
    let float_numeric = rp::seq!(float_numeric.string(), suffix);

    DynParser::new(float_numeric.map(|s: String, suffix: Option<char>| {
        let parse_res: f64 = s.parse::<f64>().expect("Failed to parse String to f64");
        let suffix = suffix.or(Some('f')).unwrap();
        match suffix {
            'f' | 'F' => Token::ConstantFloat(parse_res as f32),
            'l' | 'L' => Token::ConstantDouble(parse_res),
            _ => panic!("Invalid suffix for float"),
        }
    }))
}
