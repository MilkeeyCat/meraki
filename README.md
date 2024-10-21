# Meraki
This language feels and looks like if C and Rust had a baby and they were
ashamed of it.

## Implemented features

### Data Types
- usize
- isize
- u8
- i8(i8 is also used as `char` in C)
- u16
- i16
- u32
- i32
- u64
- i64
- bool
- void

### Functions

```rust
fn printf(format: *i8) -> void;

fn main() -> u8 {
    printf("Hello, world!");

    return 0;
}
```

### Structs

```rust
struct Foo {
    bar: u8;
    baz: *i16;

    fn useless_method() -> i16 {
        return this->bar as i16 + *this->baz;
    }
}

let tmp2: u16 = 420;
let foo: Foo = Foo {
    bar: 69,
    baz: &tmp2,
};
let res: i16 = foo.useless_method(); // 489
```

### Variables

```rust
let foo: usize = 69;
let bar: *usize= &foo;
let baz: u32[5] = [1, 2, 3, 4, 5];
```

### Casting

Use keyword `as` for casting expressions. Also we don't cast integer variables for ya.
No implicit conversions! Only integer literals can be promoted to bigger type

### Proc Macros
Currently it's not possible to declare macros from meraki but there's
[C api](https://github.com/MilkeeyCat/meraki/blob/34ed08f02b63de1fa031cea76ca8462fbab15232/src/macros/c_api.c)
following which you can create a shared object with macros and pass it to a compiler using `--macro` flag.</br>
Macro invokation syntax:</br>
```rust
foo!(I'll eat anything);
```
