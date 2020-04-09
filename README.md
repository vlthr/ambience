# Ambience

A lightweight library for passing ambient parameters.

## Examples

Thread-local ambient parameters can be set with `ambience::thread::set`. The ambient parameter can then be accessed via `ambience::thread::get` until the guard object returned by `set` is dropped.

```rust
struct User {
    // ...
}

fn process() {
    // retrieve the user from this threads ambient environment
    let user: Rc<User> = ambience::thread::get::<User>();
    // ...
}

fn main() {
    let user = // ...

    // set the user as ambient data for the current thread.
    // the value is accessible until `_ambience_guard` is dropped.
    let _ambience_guard = ambience::thread::set::<User>(user);
    process();
}
```
