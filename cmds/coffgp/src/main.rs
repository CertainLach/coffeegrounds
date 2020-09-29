use std::path::PathBuf;

use clap::Clap;
use coffeegrounds_core::class::ClassFile;

#[derive(Clap)]
struct Opts {
	file: PathBuf,
}

fn main() -> anyhow::Result<()> {
	let opts = Opts::parse();
	let data = std::fs::read(opts.file)?;
	let (rest, class) = ClassFile::parse(&data).unwrap();
	println!("{}", class);

	Ok(())
}
