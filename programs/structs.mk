struct Foo {
	u8 value;
}

struct Bar {
	Foo foo;
	u8 value;
}

struct Baz {
	Bar bar;
	u16 value;
}

u8 main() {
	Baz baz;
	baz = Baz {
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
