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
        Object::Array(array) => Object::Int(array.len().try_into().unwrap()),
        arg => Object::Error(format!("argument to `len` not supported, got {arg:?}")),
    }
}

pub fn first(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::Array(array) => match array.first() {
            Some(element) => element.clone(),
            None => Object::Null,
        },
        arg => Object::Error(format!("argument to `first` not supported, got {arg:?}")),
    }
}

pub fn last(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::Array(array) => match array.last() {
            Some(element) => element.clone(),
            None => Object::Null,
        },
        arg => Object::Error(format!("argument to `last` not supported, got {arg:?}")),
    }
}

pub fn rest(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::Array(array) => {
            if array.is_empty() {
                return Object::Null;
            }

            Object::Array(array[1..].to_vec())
        }
        arg => Object::Error(format!("argument to `rest` not supported, got {arg:?}")),
    }
}

pub fn push(args: Vec<Object>) -> Object {
    if args.len() != 2 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=2",
            args.len()
        ));
    }

    match &args[0] {
        Object::Array(array) => {
            let mut new_array = array.clone();
            new_array.push(args[1].clone());
            Object::Array(new_array)
        }
        arg => Object::Error(format!("argument to `push` must be ARRAY, got {arg:?}")),
    }
}

pub fn puts(args: Vec<Object>) -> Object {
    for arg in args {
        println!("{arg:?}");
    }

    Object::Null
}
