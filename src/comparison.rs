use super::*;
use std::io::{Error, Result};

/// Comperators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Comparator {
    /// `<` or `LT` (Less Than)
    Less,
    /// `<=` or `LE` (Less Than or Equal)
    LessOrEqual,
    /// `>` or `GT` (Greater Than)
    Greater,
    /// `>=` or `GE` (Greater Than or Equal)
    GreaterOrEqual,
    /// `==` or `EQ` (Equal)
    Equal,
    /// `!=` or `NE` (Not Equal)
    NotEqual,
}

/// A comparison between two data sources
#[derive(Debug, Clone, PartialEq)]
pub struct Comparison {
    pub(crate) left: Source,
    pub(crate) cmp: Comparator,
    pub(crate) right: Source,
}

impl IoCondition for Comparison {
    fn eval(&self, io: &IoState) -> Result<bool> {
        use Comparator::*;
        use ErrorKind::*;
        use Value::*;
        let left = get_val(&self.left, io)?;
        let right = get_val(&self.right, io)?;
        let res = match left {
            Bit(a) => {
                if let Bit(b) = right {
                    match self.cmp {
                        Equal => a == b,
                        NotEqual => a != b,
                        _ => {
                            return Err(Error::new(
                                InvalidInput,
                                format!(
                                    "Bits can't be compared with a '{:?}' comparator",
                                    self.cmp
                                ),
                            ));
                        }
                    }
                } else {
                    return Err(Error::new(
                        InvalidInput,
                        format!("Bits can only compared with other bits"),
                    ));
                }
            }
            Decimal(a) => {
                if let Decimal(b) = right {
                    match self.cmp {
                        Less => a < b,
                        LessOrEqual => a <= b,
                        Greater => a > b,
                        GreaterOrEqual => a >= b,
                        Equal => a == b,
                        NotEqual => a != b,
                    }
                } else {
                    return Err(Error::new(
                        InvalidInput,
                        format!("Decimal values can only compared with other decimals"),
                    ));
                }
            }
            Integer(a) => {
                if let Integer(b) = right {
                    match self.cmp {
                        Less => a < b,
                        LessOrEqual => a <= b,
                        Greater => a > b,
                        GreaterOrEqual => a >= b,
                        Equal => a == b,
                        NotEqual => a != b,
                    }
                } else {
                    return Err(Error::new(
                        InvalidInput,
                        format!("Integer values can only compared with other integers"),
                    ));
                }
            }

            Text(a) => {
                if let Text(b) = right {
                    match self.cmp {
                        Equal => a == b,
                        NotEqual => a != b,
                        _ => {
                            return Err(Error::new(
                                InvalidInput,
                                format!(
                                    "Text values can't be compared with a '{:?}' comparator",
                                    self.cmp
                                ),
                            ));
                        }
                    }
                } else {
                    return Err(Error::new(
                        InvalidInput,
                        format!("Text values can only compared with other text"),
                    ));
                }
            }

            Bin(a) => {
                if let Bin(b) = right {
                    match self.cmp {
                        Equal => a == b,
                        NotEqual => a != b,
                        _ => {
                            return Err(Error::new(
                                InvalidInput,
                                format!(
                                    "Binary data can't be compared with a '{:?}' comparator",
                                    self.cmp
                                ),
                            ));
                        }
                    }
                } else {
                    return Err(Error::new(
                        InvalidInput,
                        format!("Binary data can only compared with other binary data"),
                    ));
                }
            }
        };
        Ok(res)
    }
}

fn get_val<'a>(src: &'a Source, io: &'a IoState) -> Result<&'a Value> {
    use ErrorKind::*;
    use Source::*;
    match src {
        In(ref id) => io.inputs.get(id).ok_or_else(|| {
            Error::new(
                NotFound,
                format!("The state of input '{}' does not exist", id),
            )
        }),
        Out(ref id) => io.outputs.get(id).ok_or_else(|| {
            Error::new(
                NotFound,
                format!("The state of output '{}' does not exist", id),
            )
        }),
        Const(ref v) => Ok(v),
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use Comparator::*;
    use Source::*;

    #[test]
    fn create_comparison_from_value_source() {
        let input = Source::In("x".into());
        let val = Source::Const(Value::Decimal(90.0));

        let eq = input.clone().cmp_eq(val.clone());
        assert_eq!(eq.left, input);
        assert_eq!(eq.right, val);
        assert_eq!(eq.cmp, Equal);

        let le = input.clone().cmp_le(val.clone());
        assert_eq!(le.left, input);
        assert_eq!(le.right, val);
        assert_eq!(le.cmp, LessOrEqual);

        let ge = input.clone().cmp_ge(val.clone());
        assert_eq!(ge.left, input);
        assert_eq!(ge.right, val);
        assert_eq!(ge.cmp, GreaterOrEqual);

        let ne = input.clone().cmp_ne(val.clone());
        assert_eq!(ne.left, input);
        assert_eq!(ne.right, val);
        assert_eq!(ne.cmp, NotEqual);

        let lt = input.clone().cmp_lt(val.clone());
        assert_eq!(lt.left, input);
        assert_eq!(lt.right, val);
        assert_eq!(lt.cmp, Less);

        let gt = input.clone().cmp_gt(val.clone());
        assert_eq!(gt.left, input);
        assert_eq!(gt.right, val);
        assert_eq!(gt.cmp, Greater);
    }

    #[test]
    fn evaluate_comparison_with_missing_values() {
        let mut state = IoState::default();
        let cmp = In("x".into()).cmp_gt(In("y".into()));
        assert!(cmp.eval(&mut state).is_err());
        state.inputs.insert("x".into(), 5.4.into());
        assert!(cmp.eval(&mut state).is_err());
        state.inputs.remove("x");
        state.inputs.insert("y".into(), 5.4.into());
        assert!(cmp.eval(&mut state).is_err());
        state.inputs.insert("x".into(), 5.4.into());
        state.inputs.insert("y".into(), 5.4.into());
        assert!(cmp.eval(&mut state).is_ok());
    }

    #[test]
    fn evaluate_decimal_comparison() {
        let ok_tests: Vec<(Value, Comparator, Value, bool)> = vec![
            (5.4.into(), Greater, 5.4.into(), false),
            (6.0.into(), Greater, 5.4.into(), true),
            (6.0.into(), GreaterOrEqual, 5.4.into(), true),
            (5.4.into(), Less, 5.4.into(), false),
            (5.4.into(), Less, 6.0.into(), true),
            (5.4.into(), Equal, 5.4.into(), true),
            (5.4.into(), LessOrEqual, 6.0.into(), true),
        ];
        let err_tests: Vec<(Value, Comparator, Value)> = vec![
            (5.0.into(), Equal, true.into()),
            (5.0.into(), Equal, "5.0".to_string().into()),
            (5.0.into(), Equal, vec![0x05_u8].into()),
            (5.0.into(), Equal, 5.into()),
        ];
        run_cmp_ok_tests(ok_tests);
        run_cmp_err_tests(err_tests);
    }

    #[test]
    fn evaluate_bit_comparison() {
        let ok_tests: Vec<(Value, Comparator, Value, bool)> = vec![
            (true.into(), Equal, true.into(), true),
            (true.into(), NotEqual, true.into(), false),
        ];
        let err_tests: Vec<(Value, Comparator, Value)> = vec![
            (true.into(), LessOrEqual, true.into()),
            (true.into(), GreaterOrEqual, true.into()),
            (true.into(), Greater, true.into()),
            (true.into(), Less, true.into()),
            (true.into(), Equal, 5.4.into()),
            (true.into(), Equal, "true".to_string().into()),
            (true.into(), Equal, vec![0x01_u8].into()),
        ];
        run_cmp_ok_tests(ok_tests);
        run_cmp_err_tests(err_tests);
    }

    #[test]
    fn evaluate_integer_comparison() {
        let ok_tests: Vec<(Value, Comparator, Value, bool)> = vec![
            (5.into(), Greater, 4.into(), true),
            (5.into(), Greater, 5.into(), false),
            (6.into(), GreaterOrEqual, 5.into(), true),
            (5.into(), Less, 5.into(), false),
            (4.into(), Equal, 4.into(), true),
        ];
        let err_tests: Vec<(Value, Comparator, Value)> = vec![
            (5.into(), Equal, 5.0.into()),
            (5.into(), Equal, "5".to_string().into()),
            (5.into(), Equal, vec![0x05_u8].into()),
            (1.into(), Equal, true.into()),
        ];
        run_cmp_ok_tests(ok_tests);
        run_cmp_err_tests(err_tests);
    }

    #[test]
    fn evaluate_string_comparison() {
        let ok_tests: Vec<(Value, Comparator, Value, bool)> = vec![
            (
                "foo".to_string().into(),
                Equal,
                "foo".to_string().into(),
                true,
            ),
            ("5".to_string().into(), Equal, "5".to_string().into(), true),
        ];
        let err_tests: Vec<(Value, Comparator, Value)> = vec![
            ("5.0".to_string().into(), Equal, 5.0.into()),
            ("4".to_string().into(), Equal, 4.into()),
            ("5".to_string().into(), Equal, vec![0x05_u8].into()),
            ("true".to_string().into(), Equal, true.into()),
            ("foo".to_string().into(), Less, "foo".to_string().into()),
        ];
        run_cmp_ok_tests(ok_tests);
        run_cmp_err_tests(err_tests);
    }

    #[test]
    fn evaluate_byte_buffer_comparison() {
        let ok_tests: Vec<(Value, Comparator, Value, bool)> = vec![
            (
                "foo".as_bytes().to_vec().into(),
                Equal,
                Value::Bin(vec![0x66, 0x6F, 0x6F]),
                true,
            ),
            (
                "foo".as_bytes().to_vec().into(),
                NotEqual,
                Value::Bin(vec![0x66, 0x6F, 0x6F]),
                false,
            ),
        ];
        let err_tests: Vec<(Value, Comparator, Value)> = vec![
            ("5".as_bytes().to_vec().into(), Equal, 5.into()),
            ("true".as_bytes().to_vec().into(), Equal, true.into()),
            (
                "5".as_bytes().to_vec().into(),
                Less,
                "7".as_bytes().to_vec().into(),
            ),
        ];
        run_cmp_ok_tests(ok_tests);
        run_cmp_err_tests(err_tests);
    }

    fn run_cmp_ok_tests(ok_tests: Vec<(Value, Comparator, Value, bool)>) {
        let mut state = IoState::default();
        let left = In("x".into());
        let right = In("y".into());
        for (a, cmp, b, res) in ok_tests {
            let cmp = Comparison {
                left: left.clone(),
                cmp,
                right: right.clone(),
            };
            state.inputs.insert("x".into(), a);
            state.inputs.insert("y".into(), b);
            assert_eq!(cmp.eval(&mut state).unwrap(), res);
        }
    }

    fn run_cmp_err_tests(err_tests: Vec<(Value, Comparator, Value)>) {
        let mut state = IoState::default();
        let left = In("x".into());
        let right = In("y".into());
        for (a, cmp, b) in err_tests {
            let cmp = Comparison {
                left: left.clone(),
                cmp,
                right: right.clone(),
            };
            state.inputs.insert("x".into(), a);
            state.inputs.insert("y".into(), b);
            assert!(cmp.eval(&mut state).is_err());
        }
    }
}
