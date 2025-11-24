mod lex;
mod parse;
use lex::lex;
use parse::parse;

pub fn run(source: String) {
    let tokens = lex(source);
    let declarations = parse(tokens);
    for d in declarations {
        println!("{d:#?}");
    }
}
