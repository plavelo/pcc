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
        let mut pos = position;
        let mut acc: Vec<Value> = Vec::new();
        let mut result = parser.parse(&source, pos);
        if result.is_err() {
            return Ok(Success {
                position: pos,
                value: Value::List(acc),
            });
        }
        pos = result.position();
        acc.push(result.value());
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
        let regex = Regex::new(pattern).unwrap();
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
        let mut pos = position;
        let mut acc: Vec<Value> = Vec::new();
        let mut result = parser.parse(&source, pos);
        if result.is_err() {
            return Ok(Success {
                position: result.position(),
                value: Value::List(acc),
            });
        }
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
        merge_results(
            Ok(Success {
                position: pos,
                value: Value::List(acc),
            }),
            result,
        )
    }
}

fn sep_by1<'a, P, S>(parser: P, separator: S) -> impl Parser<'a>
where
    P: Parser<'a>,
    S: Parser<'a>,
{
    move |source: &'a str, position| -> Result<Success, Failure> {
        let mut pos = position;
        let mut acc: Vec<Value> = Vec::new();
        let mut result = parser.parse(&source, pos);
        if result.is_err() {
            return Err(Failure {
                position: position,
                expected: result.expected(),
            });
        }
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
        merge_results(
            Ok(Success {
                position: pos,
                value: Value::List(acc),
            }),
            result,
        )
    }
}

fn and<'a, A, B>(parser1: A, parser2: B) -> impl Parser<'a>
where
    A: Parser<'a>,
    B: Parser<'a>,
{
    move |source: &'a str, position| -> Result<Success, Failure> {
        let mut pos = position;
        let mut acc: Vec<Value> = Vec::new();
        let mut result = parser1.parse(&source, pos);
        if result.is_err() {
            return result;
        }
        pos = result.position();
        acc.push(result.value());

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

fn skip<'a, P1, P2>(first: P1, second: P2) -> impl Parser<'a>
where
    P1: Parser<'a>,
    P2: Parser<'a>,
{
    map(and(first, second), move |value: Value| match value {
        Value::List(val) => match val.first() {
            Some(v) => match v {
                Value::None => Value::None,
                other => other.clone(),
            },
            None => Value::None,
        },
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
        Value::List(val) => match val.last() {
            Some(v) => match v {
                Value::None => Value::None,
                other => other.clone(),
            },
            None => Value::None,
        },
        _ => panic!(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alt_ok() {
        let parser = or(or(string("x"), string("y")), string("z"));
        let result = parse(parser, "x");
        assert_eq!(result.is_ok(), true);
    }

    #[test]
    fn alt_error() {
        let parser = or(or(string("x"), string("y")), string("z"));
        let result = parse(parser, "x");
        assert_eq!(result.is_ok(), true);
    }

    #[test]
    fn many_ok() {
        let parser = many(string("x"));
        let result = parse(parser, "xxxxxx");
        assert_eq!(result.is_ok(), true);
    }

    #[test]
    fn many_error() {
        let parser = many(string("x"));
        let result = parse(parser, "xxxxxy");
        assert_eq!(result.is_ok(), false);
    }

    #[test]
    fn regex_ok() {
        let parser1 = regex(r"([0-9]+)([a-z]+)", 1);
        let result1 = parse(parser1, "123abc");
        assert_eq!(result1.is_ok(), true);
        assert_eq!(result1.value(), Value::Some("123".to_string()));

        let parser2 = regex(r"[0-9]+", 0);
        let result2 = parse(parser2, "123");
        assert_eq!(result2.is_ok(), true);
        assert_eq!(result2.value(), Value::Some("123".to_string()));
    }

    #[test]
    fn regex_error() {
        let parser = regex(r"[0-9]+", 0);
        let result = parse(parser, "12a");
        assert_eq!(result.is_ok(), false);
    }

    #[test]
    fn sep_by1_ok() {
        let parser1 = sep_by1(string("val"), string(","));
        let result1 = parse(parser1, "val");
        assert_eq!(result1.is_ok(), true);

        let parser2 = sep_by1(string("val"), string(","));
        let result2 = parse(parser2, "val,val,val");
        assert_eq!(result2.is_ok(), true);
    }

    #[test]
    fn sep_by1_error() {
        let parser1 = sep_by1(string("val"), string(","));
        let result1 = parse(parser1, "");
        assert_eq!(result1.is_ok(), false);

        let parser2 = sep_by1(string("val"), string(","));
        let result2 = parse(parser2, "val,");
        assert_eq!(result2.is_ok(), false);
    }

    #[test]
    fn sep_by_ok() {
        let parser1 = sep_by(string("val"), string(","));
        let result1 = parse(parser1, "");
        assert_eq!(result1.is_ok(), true);

        let parser2 = sep_by(string("val"), string(","));
        let result2 = parse(parser2, "val");
        assert_eq!(result2.is_ok(), true);

        let parser3 = sep_by(string("val"), string(","));
        let result3 = parse(parser3, "val,val,val");
        assert_eq!(result3.is_ok(), true);
    }

    #[test]
    fn sep_by_error() {
        let parser = sep_by(string("val"), string(","));
        let result = parse(parser, "val,");
        assert_eq!(result.is_ok(), false);
    }

    #[test]
    fn seq_ok() {
        let parser = and(and(string("key"), string(":")), string("value"));
        let result = parse(parser, "key:value");
        assert_eq!(result.is_ok(), true);
    }

    #[test]
    fn skip_ok() {
        let parser = skip(string("x"), string("y"));
        let result = parse(parser, "xy");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("x".to_string()));
    }

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
        assert_eq!(result.is_ok(), false);
    }

    #[test]
    fn then_ok() {
        let parser = then(string("x"), string("y"));
        let result = parse(parser, "xy");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), Value::Some("y".to_string()));
    }

    #[test]
    fn json_ok() {
        fn json_boolean<'a>() -> impl Parser<'a> {
            or(string("true"), string("false"))
        }
        assert_eq!(json_boolean().parse("true", 0).is_ok(), true);
        assert_eq!(json_boolean().parse("false", 0).is_ok(), true);

        fn json_number<'a>() -> impl Parser<'a> {
            regex("-?(0|[1-9][0-9]+)", 0)
        }
        assert_eq!(json_number().parse("-123", 0).is_ok(), true);
        assert_eq!(json_number().parse("1230", 0).is_ok(), true);

        fn json_string<'a>() -> impl Parser<'a> {
            regex("\"(.*?)\"", 1)
        }
        assert_eq!(json_string().parse("\"foobar\"", 0).is_ok(), true);
        assert_eq!(json_string().parse("\"\"", 0).is_ok(), true);

        fn json_array<'a>() -> impl Parser<'a> {
            skip(
                then(
                    string("["),
                    sep_by(json_elements(), string(",")),
                ),
                string("]"),
            )
        }
        assert_eq!(json_array().parse("[\"foo\",\"bar\"]", 0).is_ok(), true);
        assert_eq!(json_array().parse("[123,456,789]", 0).is_ok(), true);

        fn json_pair<'a>() -> impl Parser<'a> {
            and(
                skip(
                    json_string(),
                    string(":"),
                ),
                json_elements(),
            )
        }
        assert_eq!(json_pair().parse("\"key\":\"value\"", 0).is_ok(), true);
        assert_eq!(json_pair().parse("\"key\":123", 0).is_ok(), true);

        fn json_object<'a>() -> impl Parser<'a> {
            skip(
                then(
                    string("{"),
                    sep_by(json_pair(), string(",")),
                ),
                string("}"),
            )
        }
        assert_eq!(json_object().parse("{\"key\":\"value\",\"key\":123}", 0).is_ok(), true);

        struct JsonElements;
        impl<'a> Parser<'a> for JsonElements {
            fn parse(&self, input: &'a str, position: i32) -> Result<Success, Failure> {
                or(
                    json_object(),
                    or(json_array(), or(json_string(), or(json_number(), json_boolean()))),
                ).parse(input, position)
            }
        }
        fn json_elements<'a>() -> impl Parser<'a> {
            JsonElements
        }
        assert_eq!(json_elements().parse("true", 0).is_ok(), true);
        assert_eq!(json_elements().parse("false", 0).is_ok(), true);
        assert_eq!(json_elements().parse("-123", 0).is_ok(), true);
        assert_eq!(json_elements().parse("1230", 0).is_ok(), true);
        assert_eq!(json_elements().parse("\"foobar\"", 0).is_ok(), true);
        assert_eq!(json_elements().parse("\"\"", 0).is_ok(), true);
        assert_eq!(json_elements().parse("[\"foo\",\"bar\"]", 0).is_ok(), true);
        assert_eq!(json_elements().parse("[123,456,789]", 0).is_ok(), true);
        assert_eq!(json_elements().parse("\"key\":\"value\"", 0).is_ok(), true);
        assert_eq!(json_elements().parse("\"key\":123", 0).is_ok(), true);
        assert_eq!(json_elements().parse("{\"key\":\"value\",\"key\":123}", 0).is_ok(), true);

        let result = json_elements().parse("{\"key\":12345}", 0).value();
        println!("hoge");
    }
}
