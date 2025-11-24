use std::fs;

fn main() {
    let source = fs::read_to_string("../examples/variables.ga").unwrap();
    gelac::run(source);
}
