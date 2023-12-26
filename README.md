# Velect: A Vec with item selection

`velect` is a Rust crate that provides a vector-like type
with item selection functionality - which here means either
zero or one index from the vector is "selected" and can
be retrieved easily without needing to manually track
the index of the selected item

Velect does the index tracking for you - it maintains
the selected index, updating it whenever a mutation
necessitates it.

## Interface

Velect implements `Deref<Target = Vec<_>>`, which allows
you to use all non-mutating functions from `Vec` as well
as `[T]` transparently. Mutating functions (those taking
`&mut self` as a parameter) are reimplemented with
compatible interfaces, helping make `Velect` a close-to-drop-in
replacement for `Vec`.

Additionally, `Velect<T>` also implements many of the same
traits as `Vec<T>`.

## License

Velect is licensed under MPL 2.0 which allows you to use
it in your project as a library for almost any use case
with proper attribution. Modifications of Velect should
be licensed under a similar license.

## Contributing

I welcome PRs and issue creation. When contributing code
you agree to assign copyright of the code you contribute to
the project in the understanding it will remain licensed
under MPL 2.0 or another similarly weak or stronger copyleft license.
