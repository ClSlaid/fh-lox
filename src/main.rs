use std::env;
use std::io::BufRead;
use std::io::BufReader;
use std::io::BufWriter;
use std::io::Error;
use std::io::ErrorKind;

use std::io::Result;
use std::io::Write;

fn main() -> Result<()> {
    let args = env::args().collect::<Vec<String>>();
    let self_name = args[0].clone();
    if args.len() < 2 {
        println!("usage: {} <script>", self_name);
        let e = ErrorKind::InvalidInput;
        let e = Error::from(e);
        return Err(e);
    }
    let _script = args[1].clone();
    todo!("run the script");
}

/// an interactive prompt on read file and output file
/// this is to be used in the REPL, and the REPL could run on
/// files, stdin and even network sockets
fn prompt<R: BufRead, W: Write>(mut input: BufReader<R>, mut output: BufWriter<W>) -> Result<()> {
    loop {
        output.write_all(b"> ")?;
        let mut line = String::new();
        input.read_line(&mut line)?;
        if line.is_empty() {
            break;
        }
        todo!("run the line");
    }
    Ok(())
}
