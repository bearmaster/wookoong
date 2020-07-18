use nom::branch::alt;
use nom::bytes::complete::{tag,take_while1};
use nom::character::complete::{digit1, multispace0,alpha1,alphanumeric1};
use nom::combinator::{map, map_res, recognize,peek,};
use nom::multi::many0;
use nom::IResult;

use nom::sequence::{preceded, delimited,pair};
use escape8259::unescape;


use std::str;

pub mod token;
use token::Token;

//whitespace and parsing stringliterals


fn is_nonescaped_string_char(c: char) -> bool {
    let cv = c as u32;
    (cv >= 0x20) && (cv != 0x22) && (cv != 0x5C)
}

// One or more unescaped text characters
fn nonescaped_string(i: &str) -> IResult<&str, &str> {
    take_while1(is_nonescaped_string_char)
    (i)
}

fn escape_code(i: &str) -> IResult<&str, &str> {
    recognize(
        pair(
            tag("\\"),
            alt((
                tag("\""),
                tag("\\"),
                tag("/"),
                tag("b"),
                tag("f"),
                tag("n"),
                tag("r"),
                tag("t"),
                tag("u"),
            ))
        )
    )
    (i)
}

fn string_body(input: &str) -> IResult<&str, &str> {
    recognize(
        many0(
            alt((
                nonescaped_string,
                escape_code
            ))
        )
    )
    (input)
}

fn string_literal(input: &str) -> IResult<&str, String> {
    let parser = delimited(
        tag("\""),
        string_body,
        tag("\"")
    );
    map_res(parser, |s| {
        unescape(s)
    })
    (input)
}
 

fn lex_string(i:&str) -> IResult<&str, Token> {
    map(string_literal, |s| Token::StringLiteral(s))(i)
}



//operator

fn  equal_operator(i:&str) -> IResult<&str, Token> {
    map(tag("=="), |_| Token::Equal)(i)
}


fn not_equal_operator(i: &str) -> IResult<&str, Token> {
    map(tag("!="), |_| Token::NotEqual)(i)
}

fn assign_operator(i: &str) -> IResult<&str, Token> {
    map(tag("="), |_| Token::Assign)(i)
}

fn plus_operator(i: &str) -> IResult<&str, Token> {
    map(tag("+"), |_| Token::Plus)(i)
}




fn minus_operator(i:&str) -> IResult<&str, Token> {
    map(tag("-"), |_| Token::Minus)(i)
}

fn multiply_operator(i: &str) -> IResult<&str, Token> {
    map(tag("*"), |_| Token::Multiply)(i)
}

fn divide_operator(i:&str) -> IResult<&str, Token> {
    map(tag("/"), |_| Token::Divide)(i)
}

fn not_operator(i:&str) ->IResult<&str, Token> {
map(tag("!"), |_| Token::Not)(i)
}

fn greater_opeartor_equal(i:&str) -> IResult<&str, Token> {
    map(tag(">="), |_| Token::GreaterThanEqual)(i)
}

fn lesser_operator_equal(i:&str) -> IResult<&str, Token> {
    map(tag("<="), |_| Token::LessThanEqual)(i)
}

fn greater_operator(i:&str) -> IResult<&str, Token> {
    map(tag(">"), |_| Token::GreaterThan)(i)
}

fn lesser_operator(i:&str) -> IResult<&str, Token> {
    map(tag("<"), |_| Token::LessThan)(i)
}

fn lex_operator(i: &str) -> IResult<&str, Token> {
    alt((equal_operator,not_equal_operator,assign_operator, plus_operator,lesser_operator, greater_operator, lesser_operator_equal,
        greater_opeartor_equal,not_operator,divide_operator,multiply_operator, minus_operator, ))(i)
}

//

//punctuation

fn comma_punctuation(i: &str) -> IResult<&str, Token> {
    map(tag(","), |_| Token::Comma)(i)
}

fn semicolon_punctuation(i: &str) -> IResult<&str, Token> {
    map(tag(";"), |_| Token::SemiColon)(i)
}

fn lparen_punctuation(i: &str) -> IResult<&str, Token> {
    map(tag("("), |_| Token::LParen)(i)
}

fn rparen_punctuation(i: &str) -> IResult<&str, Token> {
    map(tag(")"), |_| Token::RParen)(i)
}

fn lbrace_punctuation(i: &str) -> IResult<&str, Token> {
    map(tag("{"), |_| Token::LBrace)(i)
}

fn rbrace_punctuation(i: &str) -> IResult<&str, Token> {
    map(tag("}"), |_| Token::RBrace)(i)
}

fn lbracket_punctuation(i:&str) -> IResult<&str, Token> {
    map(tag("["), |_| Token::LBracket)(i)
}

fn rbracket_punctuation(i:&str) -> IResult<&str, Token> {
    map(tag("]"), |_| Token::RBracket)(i)
}

fn lex_punctuation(i: &str) -> IResult<&str, Token> {
    alt((
        comma_punctuation,
        semicolon_punctuation,
        lparen_punctuation,
        rparen_punctuation,
        lbrace_punctuation,
        rbrace_punctuation,
        lbracket_punctuation,
        rbracket_punctuation,
    ))(i)
}

//integer parsing

// fn lex_integer_helper(i:&str) -> IResult<&str, i64> {
//     map_res(recognize(digit1), str::parse)(i)
// }

// fn lex_integer(i: &str) -> IResult<&str, Token> {
//     map(lex_integer_helper, |result| Token::IntLiteral(result))(i)
// }

fn lex_integer(i: &str) -> IResult<&str, Token> {
    map(map_res(recognize(digit1), str::parse), |result| {
        Token::IntLiteral(result)
    })(i)
}

// reserved keywords parsing 

fn parse_reserved_Function(i: &str) -> IResult<&str,Token>{
    map(tag("fn"), |_| Token::Function)(i)
}

fn parse_reserved_Let(i: &str) -> IResult<&str, Token> {
    map(tag("let"),|_| Token::Let)(i)
}

fn parse_reserved_If(i:&str) -> IResult<&str, Token> {
    map(tag("if"),|_| Token::If)(i)
}

fn parse_reserved_Else(i:&str) -> IResult<&str, Token> {
    map(tag("else"),|_| Token::Else)(i)
}

fn parse_reserved_Return(i:&str) -> IResult<&str, Token> {
    map(tag("return"),|_| Token::Return)(i)
}

fn parse_reserved_BooleanTrue(i:&str) ->IResult<&str, Token> {
    map(tag("true"), |_| Token::BoolLiteral(true))(i)
}

fn parse_reserved_BooleanFalse(i:&str) ->IResult<&str, Token> {
    map(tag("false"), |_| Token::BoolLiteral(false))(i)
}


fn parse_reserved_keywords(i:&str) -> IResult<&str, Token> {
    alt((
        parse_reserved_Function,
        parse_reserved_Let,
        parse_reserved_If,
        parse_reserved_Else,
        parse_reserved_Return,
        parse_reserved_BooleanTrue,
        parse_reserved_BooleanFalse,
    ))(i)
}

// Identifier parsing

fn ident_firstletter(i:&str) -> IResult<&str, &str>
{
    alt((
        alpha1,
        tag("_"),
    ))(i)
}

fn take_ident(i: &str) -> IResult<&str, &str> {
    preceded(peek(ident_firstletter), alphanumeric1)(i)
}

fn parse_reserved_ident(i:&str) -> IResult<&str, Token> {
    map(take_ident, |iden:&str| Token::Ident(iden.to_string()))(i)
}


// fn lex_reserved_ident_helper(i:&str) -> IResult<&str, &str> {
//     preceded(peek(alpha1), take_till1(is_alphanumeric))(i)
// }


// fn lex_reserved_ident(i:&str) -> IResult<&str, Token> {
//  map(lex_reserved_ident_helper, |iden| Token::Ident(iden.to_string()))(i)
// }

//final step

fn lex_token_helper(i: &str) -> IResult<&str, Token> {
    alt((lex_operator, lex_punctuation, lex_integer, parse_reserved_keywords, parse_reserved_ident, lex_string))(i)
}

fn lex_token(i:&str) -> IResult<&str, Token> {
    delimited(multispace0, lex_token_helper, multispace0)(i)
}
 
fn lex_tokens(i: &str) -> IResult<&str, Vec<Token>> {
    many0(lex_token)(i)
}

pub struct Lexer;

impl Lexer {
    pub fn lex_tokens(i: &str) -> IResult<&str, Vec<Token>> {
        lex_tokens(i).map(|(slice, result)| (slice, [&result[..], &vec![Token::EOF][..]].concat()))
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_lexer1() {
        let input = "= +(  ) 
        {},; \
        ";
        let (_, result) = Lexer::lex_tokens(input).unwrap();

        let expected_results = vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::SemiColon,
            Token::EOF,
        ];

        assert_eq!(result, expected_results);
    }

    #[test]
    fn test_lexer2() {
        let input = "let five = 5;\
             let ten = 10;\

             let ad1d = fn(x, y) {\
                 x + y;\
             };\

             let result = add(five, ten);"
            ;

        let (_, result) = Lexer::lex_tokens(input).unwrap();

        let expected_results = vec![
            Token::Let,
            Token::Ident("five".to_owned()),
            Token::Assign,
            Token::IntLiteral(5),
            Token::SemiColon,
            Token::Let,
            Token::Ident("ten".to_owned()),
            Token::Assign,
            Token::IntLiteral(10),
            Token::SemiColon,
            Token::Let,
            Token::Ident("ad1d".to_owned()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_owned()),
            Token::Comma,
            Token::Ident("y".to_owned()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_owned()),
            Token::Plus,
            Token::Ident("y".to_owned()),
            Token::SemiColon,
            Token::RBrace,
            Token::SemiColon,
            Token::Let,
            Token::Ident("result".to_owned()),
            Token::Assign,
            Token::Ident("add".to_owned()),
            Token::LParen,
            Token::Ident("five".to_owned()),
            Token::Comma,
            Token::Ident("ten".to_owned()),
            Token::RParen,
            Token::SemiColon,
            Token::EOF,
        ];

        assert_eq!(result, expected_results);
    }

    #[test]
    fn test_lexer3() {
        let input = "if (a == 10) {\
                return a;\
             } else if (a != 20) {\
                return !a;\
            } else if (a > 20) {\
                return -30 / 40 * 50;\
            } else if (a < 30) {\
                return true;\
            }\
            return false;\
            "
            ;

        let (_, result) = Lexer::lex_tokens(input).unwrap();

        let expected_results = vec![
            Token::If,
            Token::LParen,
            Token::Ident("a".to_owned()),
            Token::Equal,
            Token::IntLiteral(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::Ident("a".to_owned()),
            Token::SemiColon,
            Token::RBrace,
            Token::Else,
            Token::If,
            Token::LParen,
            Token::Ident("a".to_owned()),
            Token::NotEqual,
            Token::IntLiteral(20),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::Not,
            Token::Ident("a".to_owned()),
            Token::SemiColon,
            Token::RBrace,
            Token::Else,
            Token::If,
            Token::LParen,
            Token::Ident("a".to_owned()),
            Token::GreaterThan,
            Token::IntLiteral(20),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::Minus,
            Token::IntLiteral(30),
            Token::Divide,
            Token::IntLiteral(40),
            Token::Multiply,
            Token::IntLiteral(50),
            Token::SemiColon,
            Token::RBrace,
            Token::Else,
            Token::If,
            Token::LParen,
            Token::Ident("a".to_owned()),
            Token::LessThan,
            Token::IntLiteral(30),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::BoolLiteral(true),
            Token::SemiColon,
            Token::RBrace,
            Token::Return,
            Token::BoolLiteral(false),
            Token::SemiColon,
            Token::EOF,
        ];

        assert_eq!(result, expected_results);
    }

    #[test]
fn test_string() {
    // Plain Unicode strings with no escaping
    assert_eq!(lex_string(r#""""#), Ok(("", Token::StringLiteral("".into()))));
    assert_eq!(lex_string(r#""Hello""#), Ok(("", Token::StringLiteral("Hello".into()))));
    assert_eq!(lex_string(r#""の""#), Ok(("", Token::StringLiteral("の".into()))));
    assert_eq!(lex_string(r#""𝄞""#), Ok(("", Token::StringLiteral("𝄞".into()))));

    // valid 2-character escapes
    assert_eq!(lex_string(r#""  \\  ""#), Ok(("", Token::StringLiteral("  \\  ".into()))));
    assert_eq!(lex_string(r#""  \"  ""#), Ok(("", Token::StringLiteral("  \"  ".into()))));

    // valid 6-character escapes
    assert_eq!(lex_string(r#""\u0000""#), Ok(("", Token::StringLiteral("\x00".into()))));
    assert_eq!(lex_string(r#""\u00DF""#), Ok(("", Token::StringLiteral("ß".into()))));
    assert_eq!(lex_string(r#""\uD834\uDD1E""#), Ok(("", Token::StringLiteral("𝄞".into()))));

    // Invalid because surrogate characters must come in pairs
    assert!(lex_string(r#""\ud800""#).is_err());
    // Unknown 2-character escape
    assert!(lex_string(r#""\x""#).is_err());
    // Not enough hex digits
    assert!(lex_string(r#""\u""#).is_err());
    assert!(lex_string(r#""\u001""#).is_err());
    // Naked control character
    assert!(lex_string(r#""\x0a""#).is_err());
    // Not a JSON string because it's not wrapped in quotes
    assert!(lex_string("abc").is_err());
}

    #[test]
    fn string_literals() {
        let (_, result) = Lexer::lex_tokens(r#""foobar""#).unwrap();
        assert_eq!(result, vec![Token::StringLiteral("foobar".to_owned()), Token::EOF]);

        let (_, result) = Lexer::lex_tokens(r#""foo bar""#).unwrap();
        assert_eq!(result, vec![Token::StringLiteral("foo bar".to_owned()), Token::EOF]);

        let (_, result) = Lexer::lex_tokens(r#""foo\nbar""#).unwrap();
        assert_eq!(result, vec![Token::StringLiteral("foo\nbar".to_owned()), Token::EOF]);

        let (_, result) = Lexer::lex_tokens(r#""foo\tbar""#).unwrap();
        assert_eq!(result, vec![Token::StringLiteral("foo\tbar".to_owned()), Token::EOF]);

        let (_, result) = Lexer::lex_tokens(r#""foo\\\"bar""#)
            .unwrap();
        assert_eq!(result, vec![Token::StringLiteral("foo\\\"bar".to_owned()), Token::EOF]);

     
    }

}
