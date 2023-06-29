use std::env;
use std::io::BufRead;
use std::io::BufReader;
use std::io::BufWriter;



use std::io::Result;
use std::io::Write;

use fh_lox::eval;
use fh_lox::ExprParser;
use fh_lox::SourceLexer;

fn main() -> Result<()> {
    let args = env::args().collect::<Vec<String>>();
    let _self_name = args[0].clone();
    if args.len() < 2 {
        let input = BufReader::new(std::io::stdin());
        let output = BufWriter::new(std::io::stdout());
        prompt(input, output)?;
        return Ok(());
    }
    let _script = args[1].clone();
    todo!("run script");
}

/// an interactive prompt on read file and output file
/// this is to be used in the REPL, and the REPL could run on
/// files, stdin and even network sockets
fn prompt<R: BufRead, W: Write>(mut input: R, mut output: BufWriter<W>) -> Result<()> {
    let parser = ExprParser::new();
    loop {
        output.write_all(b"> ")?;
        let mut line = String::new();
        input.read_line(&mut line)?;
        if line.is_empty() {
            break;
        }
        let lexer = SourceLexer::new(&line);
        let mut errors = vec![];
        let parsed = parser.parse(&mut errors, lexer);
        let expr = match parsed {
            Ok(e) => e,
            Err(e) => {
                println!("error: {:?}", e);
                continue;
            }
        };
        let v = eval(&expr);
        let v = match v {
            Ok(v) => v,
            Err(e) => {
                println!("runtime error: {:?}", e);
                continue;
            }
        };
        println!("=> {}", v);
    }
    Ok(())
}
