use super::*;
use std::io::{Error, ErrorKind, Result};
use std::str::FromStr;

impl FromStr for Comparison {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        use Comparator::*;
        if s.trim().is_empty() {
            return Err(Error::new(ErrorKind::InvalidInput, "empty str"));
        }
        for cmp in &[GreaterOrEqual, Greater, Equal, LessOrEqual, Less, NotEqual] {
            if let Some(cmp) = check(s, *cmp)? {
                return Ok(cmp);
            }
        }
        Err(Error::new(ErrorKind::InvalidInput, "invalid comparison"))
    }
}

fn cmp_str<'a>(cmp: &Comparator) -> &'a str {
    use Comparator::*;
    match cmp {
        Less => "<",
        LessOrEqual => "<=",
        Greater => ">",
        GreaterOrEqual => ">=",
        Equal => "==",
        NotEqual => "!=",
    }
}

fn check(s: &str, cmp: Comparator) -> Result<Option<Comparison>> {
    if s.contains(cmp_str(&cmp)) {
        let vals = s.split(cmp_str(&cmp)).collect::<Vec<&str>>();
        return Ok(Some(Comparison {
            left: Source::from_str(vals[0])?,
            cmp,
            right: Source::from_str(vals[1])?,
        }));
    }
    Ok(None)
}

impl FromStr for Source {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let s = s.trim();
        if s.is_empty() {
            return Err(Error::new(ErrorKind::InvalidInput, "empty str"));
        }
        if s.contains("'") {
            return Ok(Source::Const(Value::Text(s.replace("'", ""))));
        }
        if let Ok(v) = s.parse::<i64>() {
            return Ok(Source::Const(v.into()));
        }
        if let Ok(v) = s.parse::<f64>() {
            return Ok(Source::Const(v.into()));
        }
        let s = s.to_lowercase();
        if s.contains("in.") {
            let res = s.split("in.").collect::<Vec<&str>>();
            if res.len() < 2 || res[1].is_empty() {
                return Err(Error::new(ErrorKind::InvalidInput, "invalid identifier"));
            }
            return Ok(Source::In(res[1].into()));
        }
        if s.contains("out.") {
            let res = s.split("out.").collect::<Vec<&str>>();
            if res.len() < 2 || res[1].is_empty() {
                return Err(Error::new(ErrorKind::InvalidInput, "invalid identifier"));
            }
            return Ok(Source::Out(res[1].into()));
        }
        if s.contains("true") {
            return Ok(Source::Const(true.into()));
        }
        if s.contains("false") {
            return Ok(Source::Const(false.into()));
        }
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_in_src() {
        assert!(Source::from_str("").is_err());
        assert!(Source::from_str(" ").is_err());
        assert!(Source::from_str("in.").is_err());
        assert!(Source::from_str("in. ").is_err());
        assert_eq!(Source::from_str("in.x").unwrap(), Source::In("x".into()));
        assert_eq!(Source::from_str("In.x").unwrap(), Source::In("x".into()));
        assert_eq!(Source::from_str("IN.x").unwrap(), Source::In("x".into()));
        assert_eq!(Source::from_str("in.X").unwrap(), Source::In("x".into()));
    }

    #[test]
    fn parse_out_src() {
        assert_eq!(Source::from_str("out.y").unwrap(), Source::Out("y".into()));
        assert_eq!(Source::from_str("OUT.y").unwrap(), Source::Out("y".into()));
    }

    #[test]
    fn parse_boolean_src() {
        assert_eq!(
            Source::from_str("true").unwrap(),
            Source::Const(true.into())
        );
        assert_eq!(
            Source::from_str("false").unwrap(),
            Source::Const(false.into())
        );
    }

    #[test]
    fn parse_int_src() {
        assert_eq!(Source::from_str("123").unwrap(), Source::Const(123.into()));
        assert_eq!(Source::from_str("0456").unwrap(), Source::Const(456.into()));
    }

    #[test]
    fn parse_float_src() {
        assert_eq!(
            Source::from_str("123.0").unwrap(),
            Source::Const(123.0.into())
        );
        assert_eq!(
            Source::from_str("0456.0").unwrap(),
            Source::Const(456.0.into())
        );
    }

    #[test]
    fn parse_text_src() {
        assert_eq!(
            Source::from_str("'foo Bar'").unwrap(),
            Source::Const(Value::Text("foo Bar".into()))
        );
        assert_eq!(
            Source::from_str("'in.x'").unwrap(),
            Source::Const(Value::Text("in.x".into()))
        );
    }

    #[test]
    fn parse_cmp() {
        use Comparator::*;
        use Source::*;
        assert!(Comparison::from_str("").is_err());
        assert!(Comparison::from_str(" ").is_err());
        assert!(Comparison::from_str("in.x ?? out.z").is_err());
        let tests = vec![
            (
                "in.x >= in.y",
                In("x".into()),
                GreaterOrEqual,
                In("y".into()),
            ),
            ("in.x > in.y", In("x".into()), Greater, In("y".into())),
            ("in.x == out.y", In("x".into()), Equal, Out("y".into())),
            (
                "out.z <= in.y",
                Out("z".into()),
                LessOrEqual,
                In("y".into()),
            ),
            ("out.z < in.y", Out("z".into()), Less, In("y".into())),
            ("out.z != in.y", Out("z".into()), NotEqual, In("y".into())),
        ];

        for (s, left, cmp, right) in tests {
            assert_eq!(
                Comparison::from_str(s).unwrap(),
                Comparison { left, cmp, right }
            );
        }
    }
}
