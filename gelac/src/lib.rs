mod ast;
mod lex;
mod parse;
use lex::lex;
use parse::parse;

pub fn run(source: String) {
    let tokens = lex(source);
    let tokens = match tokens {
        Err(error) => {
            eprintln!("[error] {error}");
            std::process::exit(1);
        }
        Ok(tokens) => tokens,
    };
    let statements = parse(tokens);
    for s in statements {
        let s = match s {
            Err(error) => {
                eprintln!("[error] {error}");
                std::process::exit(1);
            }
            Ok(s) => s,
        };
        println!("{s:#?}");
    }
}
