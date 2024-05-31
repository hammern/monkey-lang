use super::object::Object;

pub fn len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::String(string) => Object::Int(string.len().try_into().unwrap()),
        arg => Object::Error(format!("argument to `len` not supported, got {arg:?}")),
    }
}
