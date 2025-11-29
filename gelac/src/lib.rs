mod lex;
mod parse;
use lex::lex;
use parse::parse;

pub fn run(source: String) {
    let tokens = lex(source);
    println!("{tokens:#?}");
    let declarations = parse(tokens);
    for d in declarations {
        println!("{d:#?}");
    }
}
