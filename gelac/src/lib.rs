mod ast;
mod lex;
mod parse;
use lex::lex;
use parse::parse;

pub fn run(source: String) {
    let tokens = lex(source).unwrap();
    let statements = parse(tokens);
    for s in statements {
        println!("{s:#?}");
    }
}
