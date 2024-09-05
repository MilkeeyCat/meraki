struct Foo {
    value: u8;
}

struct Bar {
	foo: Foo;
	value: u8;
}

struct Baz {
	bar: Bar;
	value: u16;
}

fn main() -> u8 {
	let baz: Baz = Baz {
		bar: Bar {
			foo: Foo {
				value: 6,
			},
			value: 9,
		},
		value: 256,
	};
	baz.value = 42;

	return (u8)baz.value + baz.bar.value + baz.bar.foo.value;
}
