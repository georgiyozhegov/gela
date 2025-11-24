use std::{env, fs};

fn main() {
    let example = env::args().nth(1).unwrap();
    let source =
        fs::read_to_string(format!("../examples/{example}.ga")).unwrap();
    gelac::run(source);
}
