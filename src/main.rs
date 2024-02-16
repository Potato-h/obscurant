use lalrpop_util::lalrpop_mod;
use obscurant::{ast::Obfuscate, lexer::Lexer};
use std::io;

lalrpop_mod!(pub grammar);

fn main() -> io::Result<()> {
    for arg in std::env::args().skip(1) {
        println!("// Obscured {arg}");
        let source_code = std::fs::read_to_string(arg)?;
        let lexer = Lexer::new(&source_code[..]);
        let parser = grammar::UnitParser::new();
        let ast = parser.parse(lexer).unwrap();
        let obf = ast.obfuscate();
        println!("{}", obf);
    }

    Ok(())
}
