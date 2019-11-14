use std::path::PathBuf;
use std::str::FromStr;

#[derive(Debug, Clone)]
pub struct Config {
    pub rom_path: PathBuf,
    pub verify: bool,
}

#[derive(Debug, Clone)]
pub struct ConfigParseError {
    error: String,
}

#[derive(PartialEq)]
enum Flag {
    Verify,
}

impl FromStr for Flag {
    type Err = ConfigParseError;

    fn from_str(s: &str) -> Result<Flag, Self::Err> {
        if s == "-v" || s == "--verify" {
            Ok(Flag::Verify)
        }
        else {
            Err(ConfigParseError{ error: format!("Unrecognized flag '{}'", s) })
        }
    }
}

impl Config {
    fn parse_args(args: &[String]) -> Result<(Vec<Flag>, PathBuf), ConfigParseError> {
        if args.len() < 2 {
            return Err(ConfigParseError{ error: "Not enough arguments provided".to_string() });
        }

        let args = &args[1..];

        let flags = &args[..args.len() - 1];

        if !flags.iter().all(|f| f.starts_with('-')) {
            return Err(ConfigParseError{ error: "More than one path provided".to_string() });
        }

        let flags = flags.iter().map(|s| s.parse()).collect::<Result<Vec<Flag>, _>>()?;

        let path = PathBuf::from(&args[args.len() - 1]);

        Ok((flags, path))
    }

    pub fn from_args(args: &[String]) -> Result<Config, ConfigParseError> {
        let (flags, rom_path) = Config::parse_args(args)?;

        let verify = flags.iter().find(|&f| f == &Flag::Verify).is_some();

        Ok(Config {
            rom_path,
            verify, 
        })
    }
}

pub const CONTROLLER_POLL_INTERVAL: usize = 1000;
