use std::env;
use chumsky::Parser;
mod parse;
mod eval;

// https://github.com/zesterer/chumsky/blob/master/tutorial.md

fn main() {
    let path_str = env::args().nth(1).unwrap();
    let src = std::fs::read_to_string(&path_str).unwrap();

    let ast = parse::parse().parse(src)
        .expect("parse error");

    let ast_str = format!("{:#?}", ast);

    std::fs::write(
        path_str + ".ast",
        &ast_str)
        .ok(); // don't care if we failed to write

    println!("{}", ast_str);
}
