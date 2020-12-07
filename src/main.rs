
import_macro::import_dhall_schema!("./config.dhall");

fn main() {
	let input_file = std::env::args().nth(1).expect("no input file");
	let target: App = serde_dhall::from_file(input_file).parse().expect("Failed to parse");
	println!("{:?}", target);
}
