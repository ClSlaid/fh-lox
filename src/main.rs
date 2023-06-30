use std::env;
use std::io::BufRead;
use std::io::BufReader;
use std::io::BufWriter;

use std::io::Result;
use std::io::Write;

use fh_lox::eval;
use fh_lox::eval_expr;
use fh_lox::ExprParser;
use fh_lox::ProgramParser;
use fh_lox::RuntimeContext;
use fh_lox::SourceLexer;

fn main() -> Result<()> {
    let args = env::args().collect::<Vec<String>>();
    let _self_name = args[0].clone();
    if args.len() < 2 {
        // start an interactive prompt on stdin and stdout
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
    let program_parser = ProgramParser::new();
    let expr_parser = ExprParser::new();

    let mut ctx = RuntimeContext::new_stdio();
    loop {
        output.write_all(b"> ")?;
        output.flush()?;

        let mut line = String::new();
        input.read_line(&mut line)?;
        if line == "\n" {
            continue;
        }

        let line = line.trim();
        if line == "exit" {
            break;
        }

        let stmts: Vec<String> = line.split(';').map(|s| s.to_string()).collect();

        let (program, expr) = if line.ends_with(';') {
            let prog = stmts[..stmts.len()].join(";") + ";";
            (prog, String::new())
        } else {
            let last = stmts.last().unwrap();
            let prog = stmts[..stmts.len() - 1].join(";") + ";";
            let expr = last.to_string();
            (prog, expr)
        };

        let program = if program.is_empty() {
            program + ";"
        } else {
            program
        };

        let lexer = SourceLexer::new(&program);
        let mut errors = vec![];
        let parsed = program_parser.parse(&mut errors, lexer);
        let parsed_program = match parsed {
            Ok(e) => e,
            Err(e) => {
                println!("error: {:?}", e);
                continue;
            }
        };
        let v = eval(&parsed_program, &mut ctx);
        let v = match v {
            Ok(v) => v,
            Err(e) => {
                println!("runtime error: {:?}", e);
                continue;
            }
        };

        if expr.is_empty() {
            println!("=> {}", v);
            continue;
        }

        let lexer = SourceLexer::new(&expr);
        let mut errors = vec![];
        let parsed = expr_parser.parse(&mut errors, lexer);
        let parsed_expr = match parsed {
            Ok(e) => e,
            Err(e) => {
                println!("error: {:?}", e);
                continue;
            }
        };
        let v = eval_expr(&parsed_expr, &mut ctx);
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
