# Milk lang

First version of milk lang was interpreted language. It was looking like JS.
Let's be honest, nobody wants to make something which looks like JS. Now it will be 'StAtIcAlLy TyPeD' language.
It's going to look like if C and Rust had a baby. Here's a brief overview of syntax.

### Variables
All variables can be modified by default. I wanted to add keyword `mut` and i ended up with straight up rust clone

```rust
let foo: i32 = 69;

// bar will contain an address of foo
let bar: *i32 = &foo;

const im_const: u8 = 420;

// strings are... hard
// so what ive come up is. literals will have type `Str` which will have string len and pointer to first char
// you can define Str only as const otherwise you can go fuck yourself
const str: Str = "im string literal";

// this thing is modifiable
let heap_str: String = String::new("im on heap btw :smirk:");
```

### If statements

```c
if(a > 10) {
    return true;
} else {
    return false;
}
```

### Functions

```rust
fn foo(arg1: i32, arg2: bool): i32 {
    return 420;
}

fn bar(msg: Str): void {
    // do nothing xd
}

fn baz(): *void {
    let foo: *void = malloc(1);

    return foo;
}
```

### Loops

Only `for` for know

```rust
for(let i: u8 = 0; i < 10; i++) {
    // how to print shit, hello?
}
```

### Structs

```rust
struct Foo {
    bar: i32,
    baz: bool,
}

impl Foo {
    fn new(arg1: i32, arg2: bool): Self {
        return Self {
            bar: arg1,
            baz: arg2,
        };
    }

    fn modify_bar(self: *Self, new: i32): void {
        self->bar = new;
    }
}

let foo: Foo = Foo::new();

// create that son of the bitch on heap
let heap_foo: *Foo = malloc(sizeof(Foo)) as Foo;
```

### Enums

```rust
    enum Foo {
        Bar,
        Baz(Str),
    }

    impl Foo {
        fn new_bar(): Self {
            return Self::Bar;
        }
    }

    let bar: Foo = Foo::Bar;
    let baz: Foo = Foo::Baz("value");
    let new_bar: Foo = Foo::new_bar();
```
