extern crate regex;

use regex::Regex;

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
    pub position: i32,
    pub value: Value,
}

#[derive(Debug)]
struct Failure {
    pub position: i32,
    pub expected: Vec<String>,
}

trait Reply {
    fn position(&self) -> i32;
    fn err_position(&self) -> i32;
    fn value(&self) -> Value;
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

#[must_use]
fn merge_results(
    curr: Result<Success, Failure>,
    last: Result<Success, Failure>,
) -> Result<Success, Failure> {
    if curr.is_ok() {
        return curr;
    }
    if curr.err_position() == last.err_position() {
        let curr_failure = curr.unwrap_err();
        let last_failure = last.unwrap_err();
        return Err(Failure {
            position: curr_failure.position,
            expected: [curr_failure.expected, last_failure.expected].concat(),
        });
    }
    if curr.err_position() > last.err_position() {
        return curr;
    }
    last
}

trait Parser<'a> {
    fn parse(&self, input: &'a str, position: i32) -> Result<Success, Failure>;
}

impl<'a, F> Parser<'a> for F
where
    F: Fn(&'a str, i32) -> Result<Success, Failure>,
{
    fn parse(&self, input: &'a str, position: i32) -> Result<Success, Failure> {
        self(input, position)
    }
}

fn parse<'a, P>(parser: P, source: &'a str) -> Result<Success, Failure>
where
    P: Parser<'a>,
{
    let result = parser.parse(&source, 0);
    let pos = result.position();
    if result.is_err() {
        return Err(Failure {
            position: pos,
            expected: result.expected(),
        });
    }
    if pos < source.len() as i32 {
        return Err(Failure {
            position: pos,
            expected: vec!["EOF".to_string()],
        });
    }
    Ok(Success {
        position: pos,
        value: result.value(),
    })
}

fn and<'a, A, B>(parser1: A, parser2: B) -> impl Parser<'a>
where
    A: Parser<'a>,
    B: Parser<'a>,
{
    move |source: &'a str, position| -> Result<Success, Failure> {
        let mut result = parser1.parse(&source, position);
        if result.is_err() {
            return result;
        }
        let mut pos = result.position();
        let mut acc: Vec<Value> = vec![result.value()];

        result = merge_results(parser2.parse(&source, pos), result);
        if result.is_err() {
            return result;
        }
        pos = result.position();
        acc.push(result.value());

        Ok(Success {
            position: pos,
            value: Value::List(acc),
        })
    }
}

fn or<'a, P1, P2>(parser1: P1, parser2: P2) -> impl Parser<'a>
where
    P1: Parser<'a>,
    P2: Parser<'a>,
{
    move |source: &'a str, position| -> Result<Success, Failure> {
        let result = parser1.parse(&source, position);
        if result.is_ok() {
            return result;
        }
        merge_results(parser2.parse(&source, position), result)
    }
}

fn many<'a, P>(parser: P) -> impl Parser<'a>
where
    P: Parser<'a>,
{
    move |source: &'a str, position| -> Result<Success, Failure> {
        let mut result = parser.parse(&source, position);
        if result.is_err() {
            return Ok(Success {
                position: position,
                value: Value::None,
            });
        }
        let mut pos = result.position();
        let mut acc: Vec<Value> = vec![result.value()];
        loop {
            result = merge_results(parser.parse(&source, pos), result);
            if result.is_ok() {
                pos = result.position();
                acc.push(result.value());
                continue;
            }
            return Ok(Success {
                position: pos,
                value: Value::List(acc),
            });
        }
    }
}

fn map<'a, P, F>(parser: P, func: F) -> impl Parser<'a>
where
    P: Parser<'a>,
    F: Fn(Value) -> Value,
{
    move |source, position| -> Result<Success, Failure> {
        let result = parser.parse(source, position);
        if result.is_ok() {
            Ok(Success {
                position: result.position(),
                value: func(result.value()),
            })
        } else {
            result
        }
    }
}

fn regex<'a>(pattern: &'a str, group: usize) -> impl Parser<'a> {
    move |source: &'a str, position: i32| -> Result<Success, Failure> {
        let src = &source[position as usize..source.len()];
        let ptn = "^".to_string() + pattern;
        let regex = Regex::new(&ptn).unwrap();
        let captures = regex.captures(src);
        match captures {
            Some(caps) => {
                let text = caps.get(group).unwrap().as_str();
                let mat = regex.find(src).unwrap();
                Ok(Success {
                    position: position + (mat.end() - mat.start()) as i32,
                    value: Value::Some(text.to_string()),
                })
            }
            None => Err(Failure {
                position: position,
                expected: vec![pattern.to_string()],
            })
        }
    }
}

fn sep_by<'a, P, S>(parser: P, separator: S) -> impl Parser<'a>
where
    P: Parser<'a>,
    S: Parser<'a>,
{
    move |source: &'a str, position| -> Result<Success, Failure> {
        let mut result = parser.parse(&source, position);
        if result.is_err() {
            return Ok(Success {
                position: position,
                value: Value::None,
            });
        }
        let mut pos;
        let mut acc: Vec<Value> = Vec::new();
        loop {
            pos = result.position();
            acc.push(result.value());
            result = merge_results(separator.parse(&source, pos), result);
            if result.is_err() {
                break;
            }
            result = merge_results(parser.parse(&source, result.position()), result);
            if result.is_err() {
                break;
            }
        }
        Ok(Success {
            position: pos,
            value: Value::List(acc),
        })
    }
}

fn sep_by1<'a, P, S>(parser: P, separator: S) -> impl Parser<'a>
where
    P: Parser<'a>,
    S: Parser<'a>,
{
    move |source: &'a str, position| -> Result<Success, Failure> {
        let mut result = parser.parse(&source, position);
        if result.is_err() {
            return Err(Failure {
                position: position,
                expected: result.expected(),
            });
        }
        let mut pos;
        let mut acc: Vec<Value> = Vec::new();
        loop {
            pos = result.position();
            acc.push(result.value());
            result = merge_results(separator.parse(&source, pos), result);
            if result.is_err() {
                break;
            }
            result = merge_results(parser.parse(&source, result.position()), result);
            if result.is_err() {
                break;
            }
        }
        Ok(Success {
            position: pos,
            value: Value::List(acc),
        })
    }
}

fn skip<'a, P1, P2>(first: P1, second: P2) -> impl Parser<'a>
where
    P1: Parser<'a>,
    P2: Parser<'a>,
{
    map(and(first, second), move |value: Value| match value {
        Value::List(val) => val.first().unwrap().clone(),
        _ => panic!(),
    })
}

fn string<'a>(string: &'a str) -> impl Parser<'a> {
    move |source: &'a str, position| -> Result<Success, Failure> {
        let to = position + (string.len() as i32);
        if to > source.len() as i32 {
            return Err(Failure {
                position: position,
                expected: vec![string.to_string()],
            });
        }
        let head = source.get(position as usize..to as usize);
        match head {
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
        }
    }
}

fn then<'a, P1, P2>(first: P1, second: P2) -> impl Parser<'a>
where
    P1: Parser<'a>,
    P2: Parser<'a>,
{
    map(and(first, second), move |value: Value| match value {
        Value::List(val) => val.last().unwrap().clone(),
        _ => panic!(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn and_ok() {
        let parser = and(and(string("key"), string(":")), string("value"));
        let result = parse(parser, "key:value");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::List(vec![
                Value::List(vec![
                    Value::Some("key".to_string()),
                    Value::Some(":".to_string()),
                ]),
                Value::Some("value".to_string()),
            ]),
        );
    }

    #[test]
    fn and_error() {
        let parser = and(and(string("key"), string(":")), string("value"));
        let result = parse(parser, "key:valu");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.err_position(), 4);
    }

    #[test]
    fn or_ok() {
        let parser = or(or(string("x"), string("y")), string("z"));
        let result = parse(parser, "x");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("x".to_string()));
    }

    #[test]
    fn or_error() {
        let parser = or(or(string("x"), string("y")), string("z"));
        let result = parse(parser, "w");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.err_position(), 0);
    }

    #[test]
    fn many_ok() {
        let parser = many(string("xy"));
        let result = parse(parser, "xyxyxyxy");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::List(vec![
                Value::Some("xy".to_string()),
                Value::Some("xy".to_string()),
                Value::Some("xy".to_string()),
                Value::Some("xy".to_string()),
            ]),
        );

        let parser = many(string("xy"));
        let result = parse(parser, "");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::None,
        );
    }

    #[test]
    fn many_error() {
        let parser = many(string("x"));
        let result = parse(parser, "xxxxxy");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.err_position(), 5);
    }

    #[test]
    fn regex_ok() {
        let parser = regex(r"([0-9]+)([a-z]+)", 1);
        let result = parse(parser, "123abc");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("123".to_string()));

        let parser = regex(r"[0-9]+", 0);
        let result = parse(parser, "123");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("123".to_string()));
    }

    #[test]
    fn regex_error() {
        let parser = regex(r"[0-9]+", 0);
        let result = parse(parser, "12a");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.err_position(), 2);
    }

    #[test]
    fn sep_by1_ok() {
        let parser = sep_by1(string("val"), string(","));
        let result = parse(parser, "val");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::List(vec![
                Value::Some("val".to_string()),
            ]),
        );

        let parser = sep_by1(string("val"), string(","));
        let result = parse(parser, "val,val,val");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::List(vec![
                Value::Some("val".to_string()),
                Value::Some("val".to_string()),
                Value::Some("val".to_string()),
            ]),
        );
    }

    #[test]
    fn sep_by1_error() {
        let parser = sep_by1(string("val"), string(","));
        let result = parse(parser, "");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.err_position(), 0);

        let parser = sep_by1(string("val"), string(","));
        let result = parse(parser, "val,");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.err_position(), 3);
    }

    #[test]
    fn sep_by_ok() {
        let parser = sep_by(string("val"), string(","));
        let result = parse(parser, "");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::None,
        );

        let parser = sep_by(string("val"), string(","));
        let result = parse(parser, "val");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::List(vec![
                Value::Some("val".to_string()),
            ]),
        );

        let parser = sep_by(string("val"), string(","));
        let result = parse(parser, "val,val,val");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::List(vec![
                Value::Some("val".to_string()),
                Value::Some("val".to_string()),
                Value::Some("val".to_string()),
            ]),
        );
    }

    #[test]
    fn sep_by_error() {
        let parser = sep_by(string("val"), string(","));
        let result = parse(parser, "val,");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.err_position(), 3);
    }

    #[test]
    fn skip_ok() {
        let parser = skip(string("x"), string("y"));
        let result = parse(parser, "xy");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("x".to_string()));
    }

    #[test]
    fn skip_error() {
        let parser = skip(string("xxx"), string("yyy"));
        let result = parse(parser, "xxxxyy");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.err_position(), 3);
    }

    #[test]
    fn string_ok() {
        let parser = string("source");
        let result = parse(parser, "source");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("source".to_string()));
    }

    #[test]
    fn string_error() {
        let parser = string("source");
        let result = parse(parser, "other");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.err_position(), 0);
    }

    #[test]
    fn then_ok() {
        let parser = then(string("x"), string("y"));
        let result = parse(parser, "xy");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("y".to_string()));
    }

    #[test]
    fn then_error() {
        let parser = then(string("xxx"), string("yyy"));
        let result = parse(parser, "xxxxyy");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.err_position(), 3);
    }

    #[test]
    fn json_ok() {
        fn json_boolean<'a>() -> impl Parser<'a> {
            or(string("true"), string("false"))
        }

        let result = parse(json_boolean(), "true");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("true".to_string()));

        let result = parse(json_boolean(), "false");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("false".to_string()));

        fn json_number<'a>() -> impl Parser<'a> {
            regex("-?(0|[1-9][0-9]+)", 0)
        }

        let result = parse(json_number(), "-123");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("-123".to_string()));

        let result = parse(json_number(), "1230");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("1230".to_string()));

        fn json_string<'a>() -> impl Parser<'a> {
            regex("\"(.*?)\"", 1)
        }

        let result = parse(json_string(), "\"foobar\"");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("foobar".to_string()));

        let result = parse(json_string(), "\"\"");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("".to_string()));

        fn json_array<'a>() -> impl Parser<'a> {
            skip(
                then(
                    string("["),
                    sep_by(json_elements(), string(",")),
                ),
                string("]"),
            )
        }

        let result = parse(json_array(), "[\"foo\",\"bar\"]");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::List(vec![
                Value::Some("foo".to_string()),
                Value::Some("bar".to_string()),
            ]),
        );

        let result = parse(json_array(), "[123,456,789]");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::List(vec![
                Value::Some("123".to_string()),
                Value::Some("456".to_string()),
                Value::Some("789".to_string()),
            ]),
        );

        fn json_pair<'a>() -> impl Parser<'a> {
            and(
                skip(
                    json_string(),
                    string(":"),
                ),
                json_elements(),
            )
        }

        let result = parse(json_pair(), "\"key\":\"value\"");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::List(vec![
                Value::Some("key".to_string()),
                Value::Some("value".to_string()),
            ]),
        );

        let result = parse(json_pair(), "\"key\":123");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::List(vec![
                Value::Some("key".to_string()),
                Value::Some("123".to_string()),
            ]),
        );

        let result = parse(json_pair(), "\"key\":[123,456,789]");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::List(vec![
                Value::Some("key".to_string()),
                Value::List(vec![
                    Value::Some("123".to_string()),
                    Value::Some("456".to_string()),
                    Value::Some("789".to_string()),
                ]),
            ]),
        );

        fn json_object<'a>() -> impl Parser<'a> {
            skip(
                then(
                    string("{"),
                    sep_by(json_pair(), string(",")),
                ),
                string("}"),
            )
        }

        let result = parse(json_object(), "{\"key1\":\"value\",\"key2\":123}");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::List(vec![
                Value::List(vec![
                    Value::Some("key1".to_string()),
                    Value::Some("value".to_string()),
                ]),
                Value::List(vec![
                    Value::Some("key2".to_string()),
                    Value::Some("123".to_string()),
                ]),
            ]),
        );

        let result = parse(json_object(), "{\"key1\":[123,456,789],\"key2\":\"value\"}");
        assert_eq!(result.expected(), vec!["hoge"]);
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::List(vec![
                Value::List(vec![
                    Value::Some("key1".to_string()),
                    Value::List(vec![
                        Value::Some("123".to_string()),
                        Value::Some("456".to_string()),
                        Value::Some("789".to_string()),
                    ]),
                ]),
                Value::List(vec![
                    Value::Some("key2".to_string()),
                    Value::Some("value".to_string()),
                ]),
            ]),
        );

        struct JsonElements;
        impl<'a> Parser<'a> for JsonElements {
            fn parse(&self, input: &'a str, position: i32) -> Result<Success, Failure> {
                or(
                    or(
                        or(
                            or(
                                json_object(),
                                json_array(),
                            ),
                            json_string(),
                        ),
                        json_number(),
                    ),
                    json_boolean(),
                ).parse(input, position)
            }
        }
        fn json_elements<'a>() -> impl Parser<'a> {
            JsonElements
        }

        let result = parse(json_elements(), "true");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("true".to_string()));

        let result = parse(json_elements(), "false");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("false".to_string()));

        let result = parse(json_elements(), "-123");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("-123".to_string()));

        let result = parse(json_elements(), "1230");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("1230".to_string()));

        let result = parse(json_elements(), "\"foobar\"");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("foobar".to_string()));

        let result = parse(json_elements(), "\"\"");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("".to_string()));

        let result = parse(json_elements(), "[\"foo\",\"bar\"]");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::List(vec![
                Value::Some("foo".to_string()),
                Value::Some("bar".to_string()),
            ]),
        );

        let result = parse(json_elements(), "[123,456,789]");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::List(vec![
                Value::Some("123".to_string()),
                Value::Some("456".to_string()),
                Value::Some("789".to_string()),
            ]),
        );

        let result = parse(json_elements(), "{\"key\":\"value\",\"key\":123}");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::List(vec![
                Value::List(vec![
                    Value::Some("key".to_string()),
                    Value::Some("value".to_string()),
                ]),
                Value::List(vec![
                    Value::Some("key".to_string()),
                    Value::Some("123".to_string()),
                ]),
            ]),
        );
        let result = parse(json_elements(), "{\"arr\":[123,456,789]}");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::List(vec![
                Value::List(vec![
                    Value::Some("arr".to_string()),
                    Value::List(vec![
                        Value::Some("123".to_string()),
                        Value::Some("456".to_string()),
                        Value::Some("789".to_string()),
                    ]),
                ]),
            ]),
        );

        let result = parse(json_elements(), "{\"arr\":[123,456,789],\"obj\":{\"key\":\"value\",\"key\":123}}");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            Value::List(vec![
                Value::List(vec![
                    Value::Some("arr".to_string()),
                    Value::List(vec![
                        Value::Some("123".to_string()),
                        Value::Some("456".to_string()),
                        Value::Some("789".to_string()),
                    ]),
                ]),
                Value::List(vec![
                    Value::Some("obj".to_string()),
                    Value::List(vec![
                        Value::List(vec![
                            Value::Some("key".to_string()),
                            Value::Some("value".to_string()),
                        ]),
                        Value::List(vec![
                            Value::Some("key".to_string()),
                            Value::Some("123".to_string()),
                        ]),
                    ]),
                ]),
            ]),
        );
    }
}
