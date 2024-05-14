# Milk lang

It's like if C and Rust had a love child and was ashamed of it

## Rough syntax ideas

### Data Types
- usize
- isize
- u8
- i8
- u16
- i16
- u32
- i32
- u64
- i64
- bool
- char: typedef for u8? remove it?
- void: only as return type or pointer

### Structs

```c
struct Foo {
    u8 bar;
    i16 *baz;

    static Self new(u8 bar, i16 *baz_ptr) {
        return Self {
            bar,
            baz: baz_ptr
        }
    }

    i16 useless_method() {
        return (i16)self.bar + *self.baz;
    }
}

Foo foo = Foo::new();
i16 res = foo.useless_method();
```

### Typedefs

`type SmolInt = u8;`

### Casting

Programmer gotta cast types himself. No implicit conversions!

### Arrays

`int foo[] = [1,2,3,4,5];`

### Other

Everything else is the same as in C language, prolly
