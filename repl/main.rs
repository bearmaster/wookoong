extern crate monkey_lib;
extern crate rustyline;
extern crate nom;

use rustyline::completion::FilenameCompleter;
use rustyline::error::ReadlineError;
use rustyline::{Config, CompletionType, Editor};

use monkey_lib::lexer::*;
use monkey_lib::lexer::token::*;

#[cfg(unix)]
static PROMPT: &'static str = "\x1b[1;32mmonkey >>\x1b[0m ";

#[cfg(windows)]
static PROMPT: &'static str = "monkey >> ";

fn main() {

   let mut rl = Editor::<()>::new();
    //rl.set_completer(Some(c));

    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    println!();
    println!("This is the monkey language repl v0.3.0");
    println!("Press Ctrl-D or enter \"quit\" to exit.");
    println!();

    //let mut evaluator = Evaluator::new();

    loop {
        let readline = rl.readline(PROMPT);
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let tokens = Lexer::lex_tokens(&line);
                println!("{:?}", tokens);
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
        
    }
    rl.save_history("history.txt").unwrap();


    
    // loop {
    //     let readline = rl.readline(PROMPT);
    //     match readline {
    //         Ok(line) => {
    //             rl.add_history_entry(line.as_ref();
    //             let lex_tokens = Lexer::lex_tokens(&line);
    //             match lex_tokens {
    //                 Ok((_, r)) => {
    //                     let tokens = Tokens::new(&r);
    //                    // let parsed = Parser::parse_tokens(tokens);
    //                     // match parsed {
    //                     //     Ok((_, program)) => {
    //                     //         let eval = evaluator.eval_program(&program);
    //                     //         println!("{}", eval);
    //                     //     }
    //                     //     Err(Err::Error(_)) => println!("Parser error"),
    //                     //     Err(Err::Failure(_)) => println!("Parser failure"),
    //                     //     Err(Err::Incomplete(_)) => println!("Incomplete parsing"),
    //                     // }
    //                 }
    //             }
    //         }
    //         Err(ReadlineError::Interrupted) => {
    //             println!("CTRL-C");
    //             break;
    //         }
    //         Err(ReadlineError::Eof) => {
    //             println!("CTRL-D");
    //             break;
    //         }
    //         Err(err) => {
    //             println!("Error: {:?}", err);
    //             break;
    //         }
    //     }
    // }

}