use std::env;
use std::fs::read_dir;
use std::io::Write;
use std::path::PathBuf;
use std::time::Instant;

use compiler::lexer::Lexer;
use compiler::repl::REPL;

mod compiler;
mod parser;
mod recur;
mod utils;

const IMPORT_COMMAND: &'static str = "import";
const SYSTEM_STDLIB_ROOT: &str = "/usr/local/share/laxma";

fn import_roots(base: Option<&str>) -> Vec<PathBuf> {
    let mut roots: Vec<PathBuf> = Vec::new();
    if let Some(b) = base {
        roots.push(PathBuf::from(b));
    }
    if let Ok(cwd) = env::current_dir() {
        roots.push(cwd);
    }
    if let Ok(lib) = env::var("LAXMA_LIB") {
        if !lib.is_empty() {
            roots.push(PathBuf::from(lib));
        }
    }
    roots.push(PathBuf::from(SYSTEM_STDLIB_ROOT));
    roots
}

fn resolve_import(path: &str, roots: &[PathBuf]) -> Option<PathBuf> {
    let with_ext = if path.ends_with(".m") {
        path.to_string()
    } else {
        format!("{path}.m")
    };
    for root in roots {
        for cand in [with_ext.as_str(), path] {
            let p = root.join(cand);
            if p.is_file() || p.is_dir() {
                return Some(p);
            }
        }
    }
    None
}

fn dir_of(path: &str) -> &str {
    match path.rfind('/') {
        Some(i) => &path[..i],
        None => ".",
    }
}

fn execute_input(repl: &mut REPL, input: &str, base: Option<&str>) {
    let mut tokens = Lexer::token_stream(input);
    while tokens.not_done() && &tokens.peek().unwrap_str() == IMPORT_COMMAND {
        let (_, remaining) = tokens.advance();
        if remaining.not_done() {
            let (file_name, remaining) = remaining.advance();
            load_file(repl, &file_name.unwrap_str(), base);
            tokens = remaining;
        } else {
            tokens = remaining;
        }
    }

    if tokens.not_done() {
        let result = repl.handle_tokens(&tokens);
        if result.is_err() {
            println!("ERROR: {:?}", result.unwrap_err());
        }
    }
}

fn load_file(repl: &mut REPL, file_name: &str, base: Option<&str>) {
    let roots = import_roots(base);
    let path = match resolve_import(file_name, &roots) {
        Some(p) => p,
        None => {
            let searched = roots.iter().map(|r| r.display().to_string()).collect::<Vec<_>>().join(", ");
            println!("ERROR for '{file_name}': not found (searched: {searched})");
            return;
        }
    };

    if path.is_dir() {
        for entry in read_dir(&path).unwrap() {
            load_file(repl, entry.unwrap().path().to_str().unwrap(), None);
        }
    } else {
        let name = path.to_string_lossy().to_string();
        match std::fs::read_to_string(&path) {
            Ok(content) => execute_input(repl, &content, Some(dir_of(&name))),
            Err(e) => println!("ERROR for '{name}': {e}"),
        }
    }
}

fn start_repl(repl: &mut REPL) {
    loop {
        print!("laxma> ");
        std::io::stdout().flush().unwrap();

        let mut input = String::new();
        std::io::stdin()
            .read_line(&mut input)
            .expect("Invalid input!");

        let input = input.trim();
        if input == "/quit" || input == "/exit" {
            return;
        }

        let start = Instant::now();
        execute_input(repl, input, None);
        println!("Done in {}ms", start.elapsed().as_millis());
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut repl = REPL::new();
    if args.len() <= 1 {
        start_repl(&mut repl);
    } else {
        let file_name = &args[1];
        load_file(&mut repl, file_name, None);
        let args_string = args.iter().skip(2).map(String::from).collect::<Vec<String>>().join(" ");
        let result = repl.handle_input(&format!("main({args_string})"));
        if result.is_err() {
            println!("ERROR for '{}': {}", file_name, result.unwrap_err());
        }
    }
}
