use lalrpop_util::lalrpop_mod;
use obscurant::lexer::Lexer;
use std::io;

lalrpop_mod!(pub grammar);

fn main() -> io::Result<()> {
    for arg in std::env::args().skip(1) {
        println!("Parsing {arg}");
        let source_code = std::fs::read_to_string(arg)?;
        let lexer = Lexer::new(&source_code[..]);
        let parser = grammar::UnitParser::new();
        let ast = parser.parse(lexer).unwrap();
        println!("{}", ast);
    }

    Ok(())
}
