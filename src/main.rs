use std::env;

use universal::execute_file;

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        let filename = &args[1];

        execute_file(filename)?;

        Ok(())
    } else {
        println!(":D");

        Ok(())
    }
}
