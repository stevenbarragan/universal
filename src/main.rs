use std::env;

use universal::execute_file;

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = env::args().collect();
        println!("{:?}", args);

    if args.len() > 1 {
        let filename = &args[1];

        println!("{:?}", filename);

        execute_file(filename)?;

        Ok(())
    } else {
        println!(":D");

        Ok(())
    }
}
