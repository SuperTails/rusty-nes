extern crate nes;

use nes::config::Config;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let config = Config::from_args(&args).unwrap_or_else(|err| {
        println!("Error parsing arguments: {:?}", err);
        std::process::exit(1)
    });

    nes::run(&config).unwrap();
}
