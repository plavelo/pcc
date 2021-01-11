use std::rc::Rc;

fn main() {
    println!("Hello, world!");
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    None,
    Some(String),
    List(Vec<Value>),
}

#[derive(Debug)]
struct Success {
    pub position: usize,
    pub value: Value,
}

#[derive(Debug)]
struct Failure {
    pub position: usize,
    pub expected: Vec<String>,
}

trait Reply {
    fn position(&self) -> usize;
    fn err_position(&self) -> usize;
    fn value(&self) -> Value;
    fn expected(&self) -> Vec<String>;
}

impl Reply for Result<Success, Failure> {
    fn position(&self) -> usize {
        match self {
            Ok(success) => success.position,
            Err(failure) => failure.position,
        }
    }

    fn err_position(&self) -> usize {
        match self {
            Ok(_) => 0,
            Err(failure) => failure.position,
        }
    }

    fn value(&self) -> Value {
        match self {
            Ok(success) => success.value.clone(),
            Err(_) => panic!(),
        }
    }

    fn expected(&self) -> Vec<String> {
        match self {
            Ok(_) => panic!(),
            Err(failure) => failure.expected.to_vec(),
        }
    }
}

fn merge_results(
    curr: Result<Success, Failure>,
    last: Result<Success, Failure>,
) -> Result<Success, Failure> {
    if last.position() == -1 {
        return curr;
    }
    if curr.is_ok() {
        return curr;
    }
    if curr.err_position() == last.err_position() {
        let curr_reply = curr.unwrap_err();
        let last_reply = last.unwrap_err();
        return Err(Failure {
            position: curr_reply.position,
            expected: [curr_reply.expected, last_reply.expected].concat(),
        });
    }
    if curr.err_position() > last.err_position() {
        return curr;
    }
    return last;
}

type Parser = dyn Fn(&str, i32) -> Result<Success, Failure>;

fn parse(parser: Box<Parser>, source: &str) -> Result<Success, Failure> {
    skip(parser, eof())(source, 0)
}

fn eof() -> Box<Parser> {
    return Box::new(move |source, position| -> Result<Success, Failure> {
        if position < source.len() as i32 {
            Err(Failure {
                position: position,
                expected: vec!["EOF".to_string()],
            })
        } else {
            Ok(Success {
                position: position,
                value: Value::None,
            })
        }
    });
}

fn string(string: &'static str) -> Box<Parser> {
    return Box::new(move |source, position| -> Result<Success, Failure> {
        let to = position + (string.len() as i32);
        if to > source.len() as i32 {
            return Err(Failure {
                position: position,
                expected: vec![string.to_string()],
            });
        }
        let head = source.get(position as usize..to as usize);
        return match head {
            Some(s) => {
                if s == string {
                    Ok(Success {
                        position: to,
                        value: Value::Some(s.to_string()),
                    })
                } else {
                    Err(Failure {
                        position: position,
                        expected: vec![string.to_string()],
                    })
                }
            }
            None => Err(Failure {
                position: position,
                expected: vec![string.to_string()],
            }),
        };
    });
}

fn skip(curr: Box<Parser>, next: Box<Parser>) -> Box<Parser> {
    map(
        seq(vec![curr, next]),
        Box::new(|value: Value| match value {
            Value::List(val) => match val.first() {
                Some(v) => match v {
                    Value::None => Value::None,
                    Value::Some(string) => Value::Some(string.clone()),
                    Value::List(list) => Value::List(list.to_vec()),
                },
                None => Value::None,
            },
            _ => panic!(),
        }),
    )
}

fn seq(parsers: Vec<Box<Parser>>) -> Box<Parser> {
    return Box::new(move |source, position| -> Result<Success, Failure> {
        let mut reply: Result<Success, Failure> = Ok(Success {
            position: -1,
            value: Value::None,
        });
        let mut pos = position;
        let mut acc: Vec<Value> = Vec::new();
        let src = Rc::new(source);
        for parser in &parsers {
            reply = merge_results(parser(&src, pos), reply);
            if reply.is_err() {
                return reply;
            }
            pos = reply.position();
            acc.push(reply.value())
        }
        return merge_results(
            Ok(Success {
                position: pos,
                value: Value::List(acc),
            }),
            reply,
        );
    });
}

fn map(parser: Box<Parser>, func: Box<dyn Fn(Value) -> Value>) -> Box<Parser> {
    return Box::new(move |src, pos| -> Result<Success, Failure> {
        let reply = parser(src, pos);
        if reply.is_ok() {
            merge_results(
                Ok(Success {
                    position: reply.position(),
                    value: func(reply.value()),
                }),
                reply,
            )
        } else {
            reply
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string_ok() {
        let parser = string("source");
        let result = parse(parser, "source");
        assert_eq!(result.is_ok(), true);
    }

    #[test]
    fn string_error() {
        let parser = string("source");
        let result = parse(parser, "other");
        assert_eq!(result.is_err(), true);
    }

    #[test]
    fn skip_ok() {
        let parser = skip(string("x"), string("y"));
        let result = parse(parser, "xy");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.unwrap().value, Value::Some("x".to_string()));
    }

    #[test]
    fn seq_ok() {
        let parser = seq(vec![string("key"), string(":"), string("value")]);
        let result = parse(parser, "key:value");
        assert_eq!(result.is_ok(), true);
    }
}
