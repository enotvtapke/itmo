use derive_builder::Builder;

// В этом файле можно написать тестируемый код, а затем использовать `cargo expand` для отладки.

#[derive(Builder)]
pub struct Tests {
    name: String,
    opt: Option<i32>,
    args: Vec<String>,
    #[builder(each = "meme")]
    memes: Vec<u8>,
}

fn main() {
    let tests = Tests::builder()
        .name("cargo".to_owned())
        .args(vec!["build".to_owned()])
        .memes(vec![42_u8])
        .meme(13_u8)
        .build()
        .unwrap();
    println!("{}", tests.name);
    println!("{:?}", tests.args);
    println!("{:?}", tests.memes);
    println!("{:?}", tests.opt);
}
