#[macro_use]
extern crate pest_derive;

#[macro_use]
extern crate lazy_static;

#[cfg(test)]
mod tests;

mod vm;
mod parser;
mod ir_gen;
mod builtins;
mod utils;

use rustyline::{Editor, config::{self, Configurer}};

const VERSION : &str = env!("CARGO_PKG_VERSION");


fn execute(runtime: &mut vm::PrologVM, query: &str) {
    if let Err(err) = runtime.set_query_from_str(&query) {
        println!("Query Error: {:?}", err);
        return;
    }

    let mut satisfied = false;
    while let Some(results) = runtime.next() {
        match results {
            Ok(values) => {
                for (idx, result) in values.iter().enumerate() {
                    println!("{:}: {:?}", idx, result);
                }
                satisfied = true;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                return;
            }
        }
    }
    println!("{}.", satisfied);
}

fn main() {
    println!("Horn Prolog v{}\n", VERSION);

    let mut runtime = vm::PrologVM::new();

    let mut editor_conf = config::Builder::new();
    editor_conf.set_auto_add_history(true);
    
    let mut prompt = Editor::<()>::with_config(editor_conf.build());
    prompt.load_history(".history");
    let mut command: Option<String> = None;
    loop {
        let line = if command.is_none() {
            prompt.readline("?- ")
        } else {
            prompt.readline("|    ")
        };
        match line {
            Ok(line) => {
                match &mut command {
                    Some(command) => {
                        command.push_str(&line);
                    }
                    None => command = Some(line)
                }
                if let Some(buf) = &command {
                    if buf.trim_end().ends_with(".") {
                        execute(&mut runtime, &buf);
                        command = None;
                    }
                }
            }
            Err(_) => {
                prompt.save_history(".history");
                break;
            }
        }
    }
}
