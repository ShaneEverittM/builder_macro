extern crate builder;
use builder::Builder;

#[derive(Debug, Clone)]
pub enum Field {
    Int(u32),
    VarChar(String),
}

#[derive(Builder, Debug)]
pub struct Test {
    #[builder(each = "field")]
    fields: Vec<Field>,
}
fn main() {
    let builder = Test::builder()
        .field(Field::Int(3))
        .field(Field::VarChar(String::from("test")))
        .build();
    dbg!(builder);
}
