use std::rc::Rc;

fn main() {
    println!("Hello, world!");
}

#[derive(Debug)]
struct Success {
    pub position: i32,
    pub value: String,
}

#[derive(Debug)]
struct Failure {
    pub position: i32,
    pub expected: Vec<String>,
}

trait Reply {
    fn position(&self) -> i32;
    fn err_position(&self) -> i32;
    fn value(&self) -> String;
    fn expected(&self) -> Vec<String>;
}

impl Reply for Result<Success, Failure> {
    fn position(&self) -> i32 {
        match self {
            Ok(success) => success.position,
            Err(failure) => failure.position,
        }
    }

    fn err_position(&self) -> i32 {
        match self {
            Ok(_) => -1,
            Err(failure) => failure.position,
        }
    }

    fn value(&self) -> String {
        match self {
            Ok(success) => success.value.clone(),
            Err(_) => panic!(),
        }
    }

    fn expected(&self) -> Vec<String> {
        match self {
            Ok(_) => panic!(),
            Err(failure) => failure.expected.clone(),
        }
    }
}

fn merge_results(
    curr: Result<Success, Failure>,
    last: Result<Success, Failure>,
) -> Result<Success, Failure> {
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

type Parser = dyn Fn(String, i32) -> Result<Success, Failure>;

fn parse(parser: Box<Parser>, source: String) -> Result<Success, Failure> {
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
                value: "".to_string(),
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
        let head = string.get(position as usize..to as usize);
        return match head {
            Some(s) => {
                if s == string {
                    Ok(Success {
                        position: to,
                        value: s.to_string(),
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
    map(seq(vec![curr, next]), Box::new(|value: String| value))
}

fn seq(parsers: Vec<Box<Parser>>) -> Box<Parser> {
    return Box::new(move |source, position| -> Result<Success, Failure> {
        let mut reply: Result<Success, Failure> = Err(Failure {
            position: 0,
            expected: Vec::new(),
        });
        let mut pos = position;
        let mut acc: Vec<String> = Vec::new();
        let src = Rc::new(source);
        for parser in &parsers {
            reply = merge_results(parser(src.clone().to_string(), pos), reply);
            if reply.is_err() {
                return reply;
            }
            pos = reply.position();
            acc.push(reply.value())
        }
        return merge_results(
            Ok(Success {
                position: pos,
                value: acc.join(""),
            }),
            reply,
        );
    });
}

fn map(parser: Box<Parser>, func: Box<dyn Fn(String) -> String>) -> Box<Parser> {
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
        let result = parse(parser, "source".to_string());
        assert_eq!(result.is_ok(), true);
    }

    #[test]
    fn string_error() {
        let parser = string("source");
        let result = parse(parser, "other".to_string());
        assert_eq!(result.is_err(), true);
    }
}
