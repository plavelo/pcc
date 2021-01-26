use regex::Regex;

#[derive(Debug)]
struct Success<Output: Clone> {
    pub position: usize,
    pub value: Output,
}

#[derive(Debug)]
struct Failure {
    pub position: usize,
    pub expected: Vec<String>,
}

trait ParseResult<Output: Clone> {
    fn position(&self) -> usize;
    fn value(&self) -> Output;
    fn expected(&self) -> Vec<String>;
}

impl<Output: Clone> ParseResult<Output> for Result<Success<Output>, Failure> {
    fn position(&self) -> usize {
        match self {
            Ok(success) => success.position,
            Err(failure) => failure.position,
        }
    }

    fn value(&self) -> Output {
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

#[allow(dead_code)]
fn merge_results<Output>(
    curr: Result<Success<Output>, Failure>,
    last: Result<Success<Output>, Failure>,
) -> Result<Success<Output>, Failure>
where
    Output: Clone,
{
    if curr.is_err() && last.is_err() && (curr.position() == last.position()) {
        Err(Failure {
            position: curr.position(),
            expected: [curr.expected(), last.expected()].concat(),
        })
    } else {
        curr
    }
}

trait Parser<'a, Output: Clone> {
    fn parse(&self, input: &'a str, position: usize) -> Result<Success<Output>, Failure>;
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str, usize) -> Result<Success<Output>, Failure>,
    Output: Clone,
{
    fn parse(&self, input: &'a str, position: usize) -> Result<Success<Output>, Failure> {
        self(input, position)
    }
}

#[allow(dead_code)]
fn parse<'a, P, Output>(parser: P, source: &'a str) -> Result<Success<Output>, Failure>
where
    P: Parser<'a, Output>,
    Output: Clone,
{
    let result = parser.parse(&source, 0);
    let pos = result.position();
    if result.is_err() {
        return Err(Failure {
            position: pos,
            expected: result.expected(),
        });
    }
    if pos < source.len() {
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

#[allow(dead_code)]
fn and<'a, P1, P2, Output1, Output2>(
    parser1: P1,
    parser2: P2,
) -> impl Parser<'a, (Output1, Output2)>
where
    P1: Parser<'a, Output1>,
    P2: Parser<'a, Output2>,
    Output1: Clone,
    Output2: Clone,
{
    move |source: &'a str, position| -> Result<Success<(Output1, Output2)>, Failure> {
        let result = parser1.parse(&source, position);
        if result.is_err() {
            return Err(Failure {
                position: result.position(),
                expected: result.expected(),
            });
        }
        let mut pos = result.position();
        let value1 = result.value();

        let result = parser2.parse(&source, pos);
        if result.is_err() {
            return Err(Failure {
                position: result.position(),
                expected: result.expected(),
            });
        }
        pos = result.position();
        let value2 = result.value();

        Ok(Success {
            position: pos,
            value: (value1, value2),
        })
    }
}

#[allow(dead_code)]
fn or<'a, P1, P2, Output>(parser1: P1, parser2: P2) -> impl Parser<'a, Output>
where
    P1: Parser<'a, Output>,
    P2: Parser<'a, Output>,
    Output: Clone,
{
    move |source: &'a str, position| -> Result<Success<Output>, Failure> {
        let result = parser1.parse(&source, position);
        if result.is_ok() {
            return result;
        }
        merge_results(parser2.parse(&source, position), result)
    }
}

#[allow(dead_code)]
fn many<'a, P, Output>(parser: P) -> impl Parser<'a, Vec<Output>>
where
    P: Parser<'a, Output>,
    Output: Clone,
{
    move |source: &'a str, position| -> Result<Success<Vec<Output>>, Failure> {
        let mut result = parser.parse(&source, position);
        if result.is_err() {
            return Ok(Success {
                position,
                value: vec![],
            });
        }
        let mut pos = result.position();
        let mut acc: Vec<Output> = vec![result.value()];
        loop {
            result = merge_results(parser.parse(&source, pos), result);
            if result.is_ok() {
                pos = result.position();
                acc.push(result.value());
                continue;
            }
            return Ok(Success {
                position: pos,
                value: acc,
            });
        }
    }
}

#[allow(dead_code)]
fn map<'a, P, F, Input, Output>(parser: P, func: F) -> impl Parser<'a, Output>
where
    P: Parser<'a, Input>,
    F: Fn(Input) -> Output,
    Input: Clone,
    Output: Clone,
{
    move |source, position| -> Result<Success<Output>, Failure> {
        let result = parser.parse(source, position);
        if result.is_ok() {
            Ok(Success {
                position: result.position(),
                value: func(result.value()),
            })
        } else {
            Err(Failure {
                position: result.position(),
                expected: result.expected(),
            })
        }
    }
}

#[allow(dead_code)]
fn regex<'a>(pattern: &'a str, group: usize) -> impl Parser<'a, String> {
    move |source: &'a str, position| -> Result<Success<String>, Failure> {
        let src = &source[position..source.len()];
        let ptn = "^".to_string() + pattern;
        let captures = Regex::new(&ptn).unwrap().captures(src);
        match captures {
            Some(caps) => {
                let text = caps.get(group).unwrap().as_str();
                let mat = caps.get(0).unwrap();
                Ok(Success {
                    position: position + (mat.end() - mat.start()),
                    value: text.to_string(),
                })
            }
            None => Err(Failure {
                position,
                expected: vec![pattern.to_string()],
            }),
        }
    }
}

#[allow(dead_code)]
fn sep_by<'a, P, S, OutputP, OutputS>(parser: P, separator: S) -> impl Parser<'a, Vec<OutputP>>
where
    P: Parser<'a, OutputP>,
    S: Parser<'a, OutputS>,
    OutputP: Clone,
    OutputS: Clone,
{
    move |source: &'a str, position| -> Result<Success<Vec<OutputP>>, Failure> {
        let mut result = parser.parse(&source, position);
        if result.is_err() {
            return Ok(Success {
                position,
                value: vec![],
            });
        }
        let mut pos;
        let mut acc: Vec<OutputP> = vec![];
        loop {
            pos = result.position();
            acc.push(result.value());
            let sep_result = separator.parse(&source, pos);
            if sep_result.is_err() {
                break;
            }
            result = parser.parse(&source, sep_result.position());
            if result.is_err() {
                break;
            }
        }
        Ok(Success {
            position: pos,
            value: acc,
        })
    }
}

#[allow(dead_code)]
fn sep_by1<'a, P, S, OutputP, OutputS>(parser: P, separator: S) -> impl Parser<'a, Vec<OutputP>>
where
    P: Parser<'a, OutputP>,
    S: Parser<'a, OutputS>,
    OutputP: Clone,
    OutputS: Clone,
{
    move |source: &'a str, position| -> Result<Success<Vec<OutputP>>, Failure> {
        let mut result = parser.parse(&source, position);
        if result.is_err() {
            return Err(Failure {
                position,
                expected: result.expected(),
            });
        }
        let mut pos;
        let mut acc: Vec<OutputP> = vec![];
        loop {
            pos = result.position();
            acc.push(result.value());
            let sep_result = separator.parse(&source, pos);
            if result.is_err() {
                break;
            }
            result = parser.parse(&source, sep_result.position());
            if result.is_err() {
                break;
            }
        }
        Ok(Success {
            position: pos,
            value: acc,
        })
    }
}

#[allow(dead_code)]
fn skip<'a, P1, P2, Output1, Output2>(parser1: P1, parser2: P2) -> impl Parser<'a, Output1>
where
    P1: Parser<'a, Output1>,
    P2: Parser<'a, Output2>,
    Output1: Clone,
    Output2: Clone,
{
    map(and(parser1, parser2), move |value| value.0)
}

#[allow(dead_code)]
fn string<'a>(string: &'a str) -> impl Parser<'a, String> {
    move |source: &'a str, position| -> Result<Success<String>, Failure> {
        let to = position + string.len();
        if to > source.len() {
            return Err(Failure {
                position,
                expected: vec![string.to_string()],
            });
        }
        let head = source.get(position..to);
        match head.map(|s| s == string) {
            Some(true) => Ok(Success {
                position: to,
                value: head.unwrap().to_string(),
            }),
            _ => Err(Failure {
                position,
                expected: vec![string.to_string()],
            }),
        }
    }
}

#[allow(dead_code)]
fn then<'a, P1, P2, Output1, Output2>(parser1: P1, parser2: P2) -> impl Parser<'a, Output2>
where
    P1: Parser<'a, Output1>,
    P2: Parser<'a, Output2>,
    Output1: Clone,
    Output2: Clone,
{
    map(and(parser1, parser2), move |value| value.1)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn and_ok() {
        let parser = and(and(string("key"), string(":")), string("value"));
        let result = parse(parser, "key:value");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            (("key".to_string(), ":".to_string()), "value".to_string()),
        );
    }

    #[test]
    fn and_error() {
        let parser = and(and(string("key"), string(":")), string("value"));
        let result = parse(parser, "key:valu");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.position(), 4);
    }

    #[test]
    fn or_ok() {
        let parser = or(or(string("x"), string("y")), string("z"));
        let result = parse(parser, "x");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), "x".to_string());
    }

    #[test]
    fn or_error() {
        let parser = or(or(string("x"), string("y")), string("z"));
        let result = parse(parser, "w");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.position(), 0);
    }

    #[test]
    fn many_ok() {
        let parser = many(string("xy"));
        let result = parse(parser, "xyxyxyxy");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            vec![
                "xy".to_string(),
                "xy".to_string(),
                "xy".to_string(),
                "xy".to_string(),
            ],
        );

        let parser = many(string("xy"));
        let result = parse(parser, "");
        assert_eq!(result.is_ok(), true);
        let empty: Vec<String> = vec![];
        assert_eq!(result.value(), empty);
    }

    #[test]
    fn many_error() {
        let parser = many(string("x"));
        let result = parse(parser, "xxxxxy");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.position(), 5);
    }

    #[test]
    fn regex_ok() {
        let parser = regex(r"([0-9]+)([a-z]+)", 1);
        let result = parse(parser, "123abc");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), "123".to_string());

        let parser = regex(r"[0-9]+", 0);
        let result = parse(parser, "123");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), "123".to_string());
    }

    #[test]
    fn regex_error() {
        let parser = regex(r"[0-9]+", 0);
        let result = parse(parser, "12a");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.position(), 2);
    }

    #[test]
    fn sep_by1_ok() {
        let parser = sep_by1(string("val"), string(","));
        let result = parse(parser, "val");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), vec!["val".to_string()]);

        let parser = sep_by1(string("val"), string(","));
        let result = parse(parser, "val,val,val");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            vec!["val".to_string(), "val".to_string(), "val".to_string()],
        );
    }

    #[test]
    fn sep_by1_error() {
        let parser = sep_by1(string("val"), string(","));
        let result = parse(parser, "");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.position(), 0);

        let parser = sep_by1(string("val"), string(","));
        let result = parse(parser, "val,");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.position(), 3);
    }

    #[test]
    fn sep_by_ok() {
        let parser = sep_by(string("val"), string(","));
        let result = parse(parser, "");
        assert_eq!(result.is_ok(), true);
        let empty: Vec<String> = vec![];
        assert_eq!(result.value(), empty);

        let parser = sep_by(string("val"), string(","));
        let result = parse(parser, "val");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), vec!["val".to_string()]);

        let parser = sep_by(string("val"), string(","));
        let result = parse(parser, "val,val,val");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            vec!["val".to_string(), "val".to_string(), "val".to_string()],
        );
    }

    #[test]
    fn sep_by_error() {
        let parser = sep_by(string("val"), string(","));
        let result = parse(parser, "val,");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.position(), 3);
    }

    #[test]
    fn skip_ok() {
        let parser = skip(string("x"), string("y"));
        let result = parse(parser, "xy");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), "x".to_string());
    }

    #[test]
    fn skip_error() {
        let parser = skip(string("xxx"), string("yyy"));
        let result = parse(parser, "xxxxyy");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.position(), 3);
    }

    #[test]
    fn string_ok() {
        let parser = string("source");
        let result = parse(parser, "source");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), "source".to_string());
    }

    #[test]
    fn string_error() {
        let parser = string("source");
        let result = parse(parser, "other");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.position(), 0);
    }

    #[test]
    fn then_ok() {
        let parser = then(string("x"), string("y"));
        let result = parse(parser, "xy");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), "y".to_string());
    }

    #[test]
    fn then_error() {
        let parser = then(string("xxx"), string("yyy"));
        let result = parse(parser, "xxxxyy");
        assert_eq!(result.is_ok(), false);
        assert_eq!(result.position(), 3);
    }

    #[test]
    fn json_ok() {
        #[derive(PartialEq, Debug, Clone)]
        enum JsonValue {
            Number(i64),
            String(String),
            Bool(bool),
            Object(HashMap<String, JsonValue>),
            Array(Vec<JsonValue>),
        }

        fn json_boolean<'a>() -> impl Parser<'a, JsonValue> {
            map(or(string("true"), string("false")), |input| {
                if input == "true" {
                    JsonValue::Bool(true)
                } else {
                    JsonValue::Bool(false)
                }
            })
        }

        let result = parse(json_boolean(), "true");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), JsonValue::Bool(true));

        let result = parse(json_boolean(), "false");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), JsonValue::Bool(false));

        fn json_number<'a>() -> impl Parser<'a, JsonValue> {
            map(regex("-?(0|[1-9][0-9]*)", 0), |input| {
                JsonValue::Number(input.parse::<i64>().unwrap())
            })
        }

        let result = parse(json_number(), "-123");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), JsonValue::Number(-123));

        let result = parse(json_number(), "1230");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), JsonValue::Number(1230));

        fn json_string<'a>() -> impl Parser<'a, JsonValue> {
            map(regex("\"(.*?)\"", 1), |input| JsonValue::String(input))
        }

        let result = parse(json_string(), "\"foobar\"");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), JsonValue::String("foobar".to_string()));

        let result = parse(json_string(), "\"\"");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), JsonValue::String("".to_string()));

        fn json_array<'a>() -> impl Parser<'a, JsonValue> {
            map(
                skip(
                    then(string("["), sep_by(json_elements(), string(","))),
                    string("]"),
                ),
                |input| JsonValue::Array(input),
            )
        }

        let result = parse(json_array(), "[\"foo\",\"bar\"]");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            JsonValue::Array(vec![
                JsonValue::String("foo".to_string()),
                JsonValue::String("bar".to_string()),
            ]),
        );

        let result = parse(json_array(), "[123,456,789]");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            JsonValue::Array(vec![
                JsonValue::Number(123),
                JsonValue::Number(456),
                JsonValue::Number(789),
            ]),
        );

        fn json_pair<'a>() -> impl Parser<'a, (String, JsonValue)> {
            and(skip(regex("\"(.*?)\"", 1), string(":")), json_elements())
        }

        let result = parse(json_pair(), "\"key\":\"value\"");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            ("key".to_string(), JsonValue::String("value".to_string())),
        );

        let result = parse(json_pair(), "\"key\":123");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), ("key".to_string(), JsonValue::Number(123)));

        let result = parse(json_pair(), "\"key\":[123,456,789]");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            (
                "key".to_string(),
                JsonValue::Array(vec![
                    JsonValue::Number(123),
                    JsonValue::Number(456),
                    JsonValue::Number(789),
                ]),
            ),
        );

        fn json_object<'a>() -> impl Parser<'a, JsonValue> {
            map(
                skip(
                    then(string("{"), sep_by(json_pair(), string(","))),
                    string("}"),
                ),
                |input| JsonValue::Object(input.into_iter().collect()),
            )
        }

        let result = parse(json_object(), "{\"key1\":\"value\",\"key2\":123}");
        assert_eq!(result.is_ok(), true);
        let mut object = HashMap::new();
        object.insert("key1".to_string(), JsonValue::String("value".to_string()));
        object.insert("key2".to_string(), JsonValue::Number(123));
        assert_eq!(result.value(), JsonValue::Object(object));

        let result = parse(json_object(), "{\"key1\":[123,456,789],\"key2\":\"value\"}");
        assert_eq!(result.is_ok(), true);
        let mut object = HashMap::new();
        object.insert(
            "key1".to_string(),
            JsonValue::Array(vec![
                JsonValue::Number(123),
                JsonValue::Number(456),
                JsonValue::Number(789),
            ]),
        );
        object.insert("key2".to_string(), JsonValue::String("value".to_string()));
        assert_eq!(result.value(), JsonValue::Object(object));

        struct JsonElements;
        impl<'a> Parser<'a, JsonValue> for JsonElements {
            fn parse(
                &self,
                input: &'a str,
                position: usize,
            ) -> Result<Success<JsonValue>, Failure> {
                or(
                    or(
                        or(or(json_object(), json_array()), json_string()),
                        json_number(),
                    ),
                    json_boolean(),
                )
                .parse(input, position)
            }
        }
        fn json_elements<'a>() -> impl Parser<'a, JsonValue> {
            JsonElements
        }

        let result = parse(json_elements(), "true");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), JsonValue::Bool(true));

        let result = parse(json_elements(), "false");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), JsonValue::Bool(false));

        let result = parse(json_elements(), "-123");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), JsonValue::Number(-123));

        let result = parse(json_elements(), "1230");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), JsonValue::Number(1230));

        let result = parse(json_elements(), "\"foobar\"");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), JsonValue::String("foobar".to_string()));

        let result = parse(json_elements(), "\"\"");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), JsonValue::String("".to_string()));

        let result = parse(json_elements(), "[\"foo\",\"bar\"]");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            JsonValue::Array(vec![
                JsonValue::String("foo".to_string()),
                JsonValue::String("bar".to_string()),
            ]),
        );

        let result = parse(json_elements(), "[123,456,789]");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            JsonValue::Array(vec![
                JsonValue::Number(123),
                JsonValue::Number(456),
                JsonValue::Number(789),
            ]),
        );

        let result = parse(json_elements(), "{\"key1\":\"value\",\"key2\":123}");
        assert_eq!(result.is_ok(), true);
        let mut object = HashMap::new();
        object.insert("key1".to_string(), JsonValue::String("value".to_string()));
        object.insert("key2".to_string(), JsonValue::Number(123));
        assert_eq!(result.value(), JsonValue::Object(object));
        let result = parse(json_elements(), "{\"arr\":[123,456,789]}");
        assert_eq!(result.is_ok(), true);
        let mut object = HashMap::new();
        object.insert(
            "arr".to_string(),
            JsonValue::Array(vec![
                JsonValue::Number(123),
                JsonValue::Number(456),
                JsonValue::Number(789),
            ]),
        );
        assert_eq!(result.value(), JsonValue::Object(object));

        let result = parse(
            json_elements(),
            "{\"arr\":[123,456,789],\"obj\":{\"key1\":\"value\",\"key2\":123}}",
        );
        assert_eq!(result.is_ok(), true);
        let mut object1 = HashMap::new();
        let mut object2 = HashMap::new();
        object2.insert("key1".to_string(), JsonValue::String("value".to_string()));
        object2.insert("key2".to_string(), JsonValue::Number(123));
        object1.insert(
            "arr".to_string(),
            JsonValue::Array(vec![
                JsonValue::Number(123),
                JsonValue::Number(456),
                JsonValue::Number(789),
            ]),
        );
        object1.insert("obj".to_string(), JsonValue::Object(object2));
        assert_eq!(result.value(), JsonValue::Object(object1));
    }
}
