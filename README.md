# The Builder Pattern
The builder pattern allows chained method calles to be used instead of parameterized constructors for object initialization.

For example:
```rust
let obj = Object {field1: 5, field2: "Something"};
```

Could be expressed roughly as:
```rust
let obj = Object::builder().field1(5).field2("Something").build();
```

Being that this pattern is trivial to implement and quite rote, this crate contains a macro for automatically generating the required methods.

So for a given struct you could write:
```rust
use builder_macro::Builder;

#[derive(Builder)]
struct MyStruct {
  field1: usize,
  field2: String
}
```
and then:
```rust
let my_struct = MyStruct::builder().field1(5).field2("Foo".into()).build().unwrap();
```

Note the call to `unwrap()`, since you are not forced syntactically to call all the builder methods before calling `build()`, so it may fail. However, one can express optional
fields as follows:
```rust
#[derive(Builder)]
struct Optional {
  optional_name: Option<String>,
  required_age: usize
}
```
followed by:
```rust
let opt = Optional::builder().required_age(25).build().unwrap();
```
which would result in
```rust
dbg!(opt) => Optional {optional_name: None, required_age: 25}
```



