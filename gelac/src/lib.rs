mod lex;
use lex::lex;

pub fn run(source: String) {
    let tokens = lex(source);
    for token in tokens {
        println!("{token:?}");
    }
}
