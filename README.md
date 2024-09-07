# Meraki

It's like if C and Rust had a love child and was ashamed of it

## Rough syntax ideas

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

### Variables

```rust
let foo: usize = 69;
let bar: *Struct = &Struct;
let baz: u32[5] = [1, 2, 3, 4, 5];
```

### Structs

```rust
struct Foo {
    bar: u8;
    baz: *i16;

    static fn new(bar: u8, baz_ptr: *i16): Self {
        return Self {
            bar,
            baz: baz_ptr
        };
    }

    fn useless_method(): i16 {
        return (i16)self.bar + *self.baz;
    }
}

let foo: Foo = Foo::new();
let res: i16 = foo.useless_method();
```

### Typedefs

```rust
type SmolInt = u8;
```

### Casting

Use keyword `as` for casting expressions. Also we don't cast integer variables for ya.
No implicit conversions! Only integer literals can be promoted to bigger type
