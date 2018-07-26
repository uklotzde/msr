use std::fmt::{Display, Error, Formatter};
use std::ops::{Bound, Deref};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum IntervalValue<T> {
    Below(T),  // outside and strictly below the lower boundary
    Inside(T), // inside bothe boundaries of the interval
    Above(T),  // outside and strictly above the upper boundary
}

impl<T> Deref for IntervalValue<T> {
    type Target = T;

    fn deref(&self) -> &T {
        match &self {
            IntervalValue::Below(value) => &value,
            IntervalValue::Inside(value) => &value,
            IntervalValue::Above(value) => &value,
        }
    }
}

/// An immutable interval of values, optionally limited by lower and upper bounds.
/// See also: https://en.wikipedia.org/wiki/Interval_(mathematics)
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Interval<T> {
    lower_bound: Bound<T>,
    upper_bound: Bound<T>,
}

impl<T> Interval<T> {
    /// Getter for the lower bound.
    pub fn lower_bound(&self) -> &Bound<T> {
        &self.lower_bound
    }

    /// Getter for the upper bound.
    pub fn upper_bound(&self) -> &Bound<T> {
        &self.upper_bound
    }

    /// Creates a new interval from lower and upper bounds.
    pub fn new(lower_bound: Bound<T>, upper_bound: Bound<T>) -> Self {
        Self {
            lower_bound,
            upper_bound,
        }
    }

    /// Creates an interval with both open lower and upper bounds.
    pub fn open<L: Into<T>, U: Into<T>>(lower: L, upper: U) -> Self {
        Self {
            lower_bound: Bound::Excluded(lower.into()),
            upper_bound: Bound::Excluded(upper.into()),
        }
    }

    /// Creates an interval with both closed lower and upper bounds.
    pub fn closed<L: Into<T>, U: Into<T>>(lower: L, upper: U) -> Self {
        Self {
            lower_bound: Bound::Included(lower.into()),
            upper_bound: Bound::Included(upper.into()),
        }
    }

    /// Creates an interval with an open lower and a closed upper bounds.
    pub fn open_closed<L: Into<T>, U: Into<T>>(lower: L, upper: U) -> Self {
        Self {
            lower_bound: Bound::Excluded(lower.into()),
            upper_bound: Bound::Included(upper.into()),
        }
    }

    /// Creates an interval with a closed lower and an open upper bound.
    pub fn closed_open<L: Into<T>, U: Into<T>>(lower: L, upper: U) -> Self {
        Self {
            lower_bound: Bound::Included(lower.into()),
            upper_bound: Bound::Excluded(upper.into()),
        }
    }

    /// Creates an unbounded interval with an exclusive lower bound.
    pub fn greater_than<L: Into<T>>(lower: L) -> Self {
        Self {
            lower_bound: Bound::Excluded(lower.into()),
            upper_bound: Bound::Unbounded,
        }
    }

    /// Creates an unbounded interval with an inclusive lower bound.
    pub fn at_least<L: Into<T>>(lower: L) -> Self {
        Self {
            lower_bound: Bound::Included(lower.into()),
            upper_bound: Bound::Unbounded,
        }
    }

    /// Creates an unbounded interval with an exclusive upper bound.
    pub fn less_than<U: Into<T>>(upper: U) -> Self {
        Self {
            lower_bound: Bound::Unbounded,
            upper_bound: Bound::Excluded(upper.into()),
        }
    }

    /// Creates an unbounded interval with an inclusive upper bound.
    pub fn at_most<U: Into<T>>(upper: U) -> Self {
        Self {
            lower_bound: Bound::Unbounded,
            upper_bound: Bound::Included(upper.into()),
        }
    }
}

impl<T> Interval<T>
where
    T: Copy,
{
    /// An unbounded interval with neither a lower nor an upper bound.
    pub const UNBOUNDED: Self = Self {
        lower_bound: Bound::Unbounded,
        upper_bound: Bound::Unbounded,
    };

    /// Creates an interval containing just a single value.
    pub fn singleton(value: T) -> Self {
        Self {
            lower_bound: Bound::Included(value),
            upper_bound: Bound::Included(value),
        }
    }
}

impl<T> Interval<T>
where
    T: PartialEq,
{
    /// Returns `true` if the interval contains only a single value.
    pub fn is_singleton(&self) -> bool {
        if let (Bound::Included(lower), Bound::Included(upper)) =
            (&self.lower_bound, &self.upper_bound)
        {
            lower == upper
        } else {
            false
        }
    }

    /// Returns `true` if interval has both lower and upper bounds.
    pub fn is_bounded(&self) -> bool {
        self.lower_bound != Bound::Unbounded && self.upper_bound != Bound::Unbounded
    }
}

impl<T> Interval<T>
where
    T: PartialOrd,
{
    /// Returns `true` if the interval is empty, i.e. does not contain any values.
    pub fn is_empty(&self) -> bool {
        match (&self.lower_bound, &self.upper_bound) {
            (Bound::Unbounded, _) => false,
            (_, Bound::Unbounded) => false,
            (Bound::Included(lower), Bound::Included(upper)) => lower > upper,
            (Bound::Included(lower), Bound::Excluded(upper)) => lower >= upper,
            (Bound::Excluded(lower), Bound::Included(upper)) => lower >= upper,
            (Bound::Excluded(lower), Bound::Excluded(upper)) => lower >= upper,
        }
    }

    /// Returns `true` if the boundaries of the interval are in order, i.e.
    /// if the lower bound is not greater than the upper bound. Invalid
    /// intervals are always empty.
    pub fn is_valid(&self) -> bool {
        match (&self.lower_bound, &self.upper_bound) {
            (Bound::Unbounded, _) => true,
            (_, Bound::Unbounded) => true,
            (Bound::Included(lower), Bound::Included(upper)) => lower <= upper,
            (Bound::Included(lower), Bound::Excluded(upper)) => lower <= upper,
            (Bound::Excluded(lower), Bound::Included(upper)) => lower <= upper,
            (Bound::Excluded(lower), Bound::Excluded(upper)) => lower <= upper,
        }
    }
}

impl<T> Interval<T>
where
    T: Copy + PartialOrd,
{
    /// Captures a given value into the boundaries of the interval.
    pub fn capture<V: Into<T>>(&self, value: V) -> IntervalValue<T> {
        debug_assert!(self.is_valid());
        let value = value.into();
        match self.lower_bound {
            Bound::Included(lower) if value < lower => {
                return IntervalValue::Below(lower);
            }
            Bound::Excluded(lower) if value <= lower => {
                return IntervalValue::Below(lower);
            }
            _ => {}
        }
        match self.upper_bound {
            Bound::Included(upper) if value > upper => {
                return IntervalValue::Above(upper);
            }
            Bound::Excluded(upper) if value >= upper => {
                return IntervalValue::Above(upper);
            }
            _ => {}
        }
        IntervalValue::Inside(value)
    }

    /// Clamps a given value to within the boundaries of the interval.
    /// If the returned value is not contained in the interval then
    /// it equals an open boundary. The result is undefined if the
    /// interval is not valid.
    pub fn clamp<V: Into<T>>(&self, value: V) -> T {
        *self.capture(value)
    }

    /// Returns `true` if the interval contains the given value.
    pub fn contains<V: Into<T>>(&self, value: V) -> bool {
        if let IntervalValue::Inside(_) = self.capture(value) {
            true
        } else {
            false
        }
    }

    pub fn interior(&self) -> Self {
        let lower_bound = match self.lower_bound {
            Bound::Included(lower) => Bound::Excluded(lower),
            lower_bound => lower_bound,
        };
        let upper_bound = match self.upper_bound {
            Bound::Included(upper) => Bound::Excluded(upper),
            upper_bound => upper_bound,
        };
        Self {
            lower_bound,
            upper_bound,
        }
    }

    pub fn closure(&self) -> Self {
        let lower_bound = match self.lower_bound {
            Bound::Excluded(lower) => Bound::Included(lower),
            lower_bound => lower_bound,
        };
        let upper_bound = match self.upper_bound {
            Bound::Excluded(upper) => Bound::Included(upper),
            upper_bound => upper_bound,
        };
        Self {
            lower_bound,
            upper_bound,
        }
    }

    /// Computes the intersection of this and another interval.
    /// The result is undefined if any of both intervals is not
    /// valid. The result might be invalid if the intersection
    /// is empty.
    pub fn intersection(&self, other: &Self) -> Self {
        debug_assert!(self.is_valid());
        debug_assert!(other.is_valid());
        let lower_bound = match self.lower_bound {
            Bound::Included(self_lower) => match other.lower_bound {
                Bound::Included(other_lower) | Bound::Excluded(other_lower) => {
                    if other_lower >= self_lower {
                        other.lower_bound
                    } else {
                        self.lower_bound
                    }
                }
                Bound::Unbounded => self.lower_bound,
            },
            Bound::Excluded(self_lower) => match other.lower_bound {
                Bound::Included(other_lower) | Bound::Excluded(other_lower) => {
                    if self_lower >= other_lower {
                        self.lower_bound
                    } else {
                        other.lower_bound
                    }
                }
                Bound::Unbounded => self.lower_bound,
            },
            Bound::Unbounded => other.lower_bound,
        };
        let upper_bound = match self.upper_bound {
            Bound::Included(self_upper) => match other.upper_bound {
                Bound::Included(other_upper) | Bound::Excluded(other_upper) => {
                    if other_upper <= self_upper {
                        other.upper_bound
                    } else {
                        self.upper_bound
                    }
                }
                Bound::Unbounded => self.upper_bound,
            },
            Bound::Excluded(self_upper) => match other.upper_bound {
                Bound::Included(other_upper) | Bound::Excluded(other_upper) => {
                    if self_upper <= other_upper {
                        self.upper_bound
                    } else {
                        other.upper_bound
                    }
                }
                Bound::Unbounded => self.upper_bound,
            },
            Bound::Unbounded => other.upper_bound,
        };
        Self {
            lower_bound,
            upper_bound,
        }
    }

    /// Returns `true` if the span of both intervals equals their union.
    /// The result is undefined if any of both intervals is not valid.
    pub fn is_connected(&self, other: &Self) -> bool {
        let intersection = self.intersection(other);
        if intersection.is_empty() {
            match (&intersection.lower_bound, &intersection.upper_bound) {
                (Bound::Unbounded, _) => true,
                (_, Bound::Unbounded) => true,
                (Bound::Included(lower), Bound::Included(upper)) => lower <= upper,
                (Bound::Included(lower), Bound::Excluded(upper)) => lower <= upper,
                (Bound::Excluded(lower), Bound::Included(upper)) => lower <= upper,
                (Bound::Excluded(lower), Bound::Excluded(upper)) => lower < upper,
            }
        } else {
            true
        }
    }

    /// Computes the minimal interval that encloses both this
    /// and another interval. The result is undefined if any
    /// of both intervals is not valid.
    pub fn span(&self, other: &Self) -> Self {
        debug_assert!(self.is_valid());
        debug_assert!(other.is_valid());
        let lower_bound = match self.lower_bound {
            Bound::Included(self_lower) => match other.lower_bound {
                Bound::Included(other_lower) | Bound::Excluded(other_lower) => {
                    if self_lower <= other_lower {
                        self.lower_bound
                    } else {
                        other.lower_bound
                    }
                }
                Bound::Unbounded => other.lower_bound,
            },
            Bound::Excluded(self_lower) => match other.lower_bound {
                Bound::Included(other_lower) | Bound::Excluded(other_lower) => {
                    if other_lower <= self_lower {
                        other.lower_bound
                    } else {
                        self.lower_bound
                    }
                }
                Bound::Unbounded => other.lower_bound,
            },
            Bound::Unbounded => self.lower_bound,
        };
        let upper_bound = match self.upper_bound {
            Bound::Included(self_upper) => match other.upper_bound {
                Bound::Included(other_upper) | Bound::Excluded(other_upper) => {
                    if self_upper >= other_upper {
                        self.upper_bound
                    } else {
                        other.upper_bound
                    }
                }
                Bound::Unbounded => other.upper_bound,
            },
            Bound::Excluded(self_upper) => match other.upper_bound {
                Bound::Included(other_upper) | Bound::Excluded(other_upper) => {
                    if other_upper >= self_upper {
                        other.upper_bound
                    } else {
                        self.upper_bound
                    }
                }
                Bound::Unbounded => other.upper_bound,
            },
            Bound::Unbounded => self.upper_bound,
        };
        Self {
            lower_bound,
            upper_bound,
        }
    }
}

impl<T> Interval<T>
where
    T: Copy + PartialEq + PartialOrd,
{
    /// Returns `true` if this interval completely encloses another interval.
    /// The result is undefined if any of both intervals is not valid.
    pub fn encloses(&self, other: &Self) -> bool {
        let intersection = self.intersection(other);
        if intersection.is_valid() {
            self == &intersection
        } else {
            false
        }
    }
}

impl<T> Default for Interval<T>
where
    T: Copy,
{
    fn default() -> Self {
        Interval::UNBOUNDED
    }
}

impl<T> Display for Interval<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let (lower_bracket, lower_value) = match &self.lower_bound {
            Bound::Included(ref value) => ('[', format!("{}", value)),
            Bound::Excluded(ref value) => (']', format!("{}", value)),
            Bound::Unbounded => (']', "-\u{221E}".into()),
        };
        let (upper_bracket, upper_value) = match &self.upper_bound {
            Bound::Included(ref value) => (']', format!("{}", value)),
            Bound::Excluded(ref value) => ('[', format!("{}", value)),
            Bound::Unbounded => ('[', "+\u{221E}".into()),
        };
        write!(
            f,
            "{}{}, {}{}",
            lower_bracket, lower_value, upper_value, upper_bracket
        )
    }
}

/// A serializable boundary.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(deny_unknown_fields))]
pub enum Boundary<T> {
    /// Inclusive boundary.
    #[cfg_attr(feature = "serde", serde(rename = "incl"))]
    Inclusive(T),

    /// Exclusive boundary.
    #[cfg_attr(feature = "serde", serde(rename = "excl"))]
    Exclusive(T),
}

impl<T> Into<Bound<T>> for Boundary<T> {
    fn into(self) -> Bound<T> {
        match self {
            Boundary::Inclusive(val) => Bound::Included(val),
            Boundary::Exclusive(val) => Bound::Excluded(val),
        }
    }
}

impl<T> Boundary<T> {
    pub fn from_bound(bound: Bound<T>) -> Option<Self> {
        match bound {
            Bound::Included(val) => Some(Boundary::Inclusive(val)),
            Bound::Excluded(val) => Some(Boundary::Exclusive(val)),
            Bound::Unbounded => None,
        }
    }
}

/// Concise serializable interval boundaries. Use this type for
/// (de-)serializing intervals with serde. Both `Interval` and
/// `IntervalBoundaries` can be converted into each other.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(deny_unknown_fields))]
pub struct IntervalBoundaries<T> {
    /// Optional lower boundary.
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Option::is_none"))]
    pub lb: Option<Boundary<T>>,

    /// Optional upper boundary.
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Option::is_none"))]
    pub ub: Option<Boundary<T>>,
}

impl<T> From<Interval<T>> for IntervalBoundaries<T> {
    fn from(from: Interval<T>) -> Self {
        let lb = Boundary::from_bound(from.lower_bound);
        let ub = Boundary::from_bound(from.upper_bound);
        Self { lb, ub }
    }
}

impl<T> From<IntervalBoundaries<T>> for Interval<T> {
    fn from(from: IntervalBoundaries<T>) -> Self {
        let lower_bound = match from.lb {
            None => Bound::Unbounded,
            Some(boundary) => boundary.into(),
        };
        let upper_bound = match from.ub {
            None => Bound::Unbounded,
            Some(boundary) => boundary.into(),
        };
        Self {
            lower_bound,
            upper_bound,
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    use std::i32;

    #[test]
    fn default() {
        assert_eq!(Interval::<f64>::UNBOUNDED, Interval::<f64>::default());
        assert_eq!(Interval::<u64>::UNBOUNDED, Interval::<u64>::default());
        assert_eq!(Interval::<i64>::UNBOUNDED, Interval::<i64>::default());
    }

    #[test]
    fn is_valid() {
        assert!(!Interval::<f64>::open_closed(1.0, -1.0).is_valid());
        assert!(Interval::<f64>::open_closed(-1.0, -1.0).is_valid());
        assert!(Interval::<f64>::closed_open(1.0, 1.0).is_valid());
        assert!(Interval::<f64>::closed_open(-1.0, 1.0).is_valid());
    }

    #[test]
    fn is_empty() {
        assert!(Interval::<f64>::open_closed(-1.0, -1.0).is_empty());
        assert!(Interval::<f64>::closed_open(1.0, 1.0).is_empty());
        assert!(!Interval::<f64>::closed(-1.0, -1.0).is_empty());
        assert!(Interval::<f64>::open(1.0, 1.0).is_empty());
    }

    #[test]
    fn is_connected() {
        assert!(Interval::<f64>::closed(-1.0, 0.0).is_connected(&Interval::open(0.0, 1.0)));
        assert!(Interval::<f64>::open(-1.0, 0.0).is_connected(&Interval::closed(0.0, 1.0)));
        assert!(!Interval::<f64>::closed(-1.0, 0.0).is_connected(&Interval::closed(1.0, 1.0)));
        assert!(!Interval::<f64>::at_most(0.0).is_connected(&Interval::closed(1.0, 1.0)));
        assert!(!Interval::<f64>::closed(-1.0, 0.0).is_connected(&Interval::at_least(1.0)));

        // ]-inf, 0[ and ]0, +inf[ are not connected, because 0 separates them!
        assert!(!Interval::<f64>::less_than(0.0).is_connected(&Interval::greater_than(0.0)));
    }

    #[test]
    fn singleton() {
        let only_five = Interval::singleton(5);
        assert!(only_five.is_valid());
        assert!(!only_five.is_empty());
        assert!(only_five.is_bounded());
        assert!(only_five.is_singleton());
        assert!(only_five.contains(5));
        assert!(!only_five.contains(4));
        assert!(!only_five.contains(6));
        assert!(!only_five.contains(0));
        assert!(!only_five.contains(-5));
        assert_eq!(5, only_five.clamp(i32::MIN));
        assert_eq!(5, only_five.clamp(4));
        assert_eq!(5, only_five.clamp(5));
        assert_eq!(5, only_five.clamp(6));
        assert_eq!(5, only_five.clamp(i32::MAX));
    }

    #[test]
    fn open() {
        let two_to_five = Interval::<i32>::open(2, 5);
        assert!(two_to_five.is_valid());
        assert!(!two_to_five.is_empty());
        assert!(two_to_five.is_bounded());
        assert!(!two_to_five.is_singleton());
        assert!(!two_to_five.contains(1));
        assert!(!two_to_five.contains(2));
        assert!(two_to_five.contains(3));
        assert!(two_to_five.contains(4));
        assert!(!two_to_five.contains(5));
        assert!(!two_to_five.contains(6));
        assert_eq!(2, two_to_five.clamp(1));
        assert_eq!(2, two_to_five.clamp(2));
        assert_eq!(3, two_to_five.clamp(3));
        assert_eq!(4, two_to_five.clamp(4));
        assert_eq!(5, two_to_five.clamp(5));
        assert_eq!(5, two_to_five.clamp(6));
    }

    #[test]
    fn closed() {
        let two_to_five = Interval::<i32>::closed(2, 5);
        assert!(two_to_five.is_valid());
        assert!(!two_to_five.is_empty());
        assert!(two_to_five.is_bounded());
        assert!(!two_to_five.is_singleton());
        assert!(!two_to_five.contains(1));
        assert!(two_to_five.contains(2));
        assert!(two_to_five.contains(3));
        assert!(two_to_five.contains(4));
        assert!(two_to_five.contains(5));
        assert!(!two_to_five.contains(6));
        assert_eq!(2, two_to_five.clamp(1));
        assert_eq!(2, two_to_five.clamp(2));
        assert_eq!(3, two_to_five.clamp(3));
        assert_eq!(4, two_to_five.clamp(4));
        assert_eq!(5, two_to_five.clamp(5));
        assert_eq!(5, two_to_five.clamp(6));
    }

    #[test]
    fn unbounded() {
        let unbounded = Interval::<i32>::UNBOUNDED;
        assert_eq!(unbounded, Interval::default());
        assert!(unbounded.is_valid());
        assert!(!unbounded.is_empty());
        assert!(!unbounded.is_bounded());
        assert!(!unbounded.is_singleton());
        assert!(unbounded.contains(i32::MIN));
        assert!(unbounded.contains(-1));
        assert!(unbounded.contains(0));
        assert!(unbounded.contains(1));
        assert!(unbounded.contains(i32::MAX));
        assert_eq!(i32::MIN, unbounded.clamp(i32::MIN));
        assert_eq!(0, unbounded.clamp(0));
        assert_eq!(i32::MAX, unbounded.clamp(i32::MAX));
    }

    #[test]
    fn capture() {
        assert_eq!(
            IntervalValue::Inside(0.0),
            Interval::<f64>::open(-1.0, 2.0).capture(0.0)
        );
        assert_eq!(
            IntervalValue::Inside(0.0),
            Interval::<f64>::closed(-1.0, 2.0).capture(0.0)
        );
        assert_eq!(
            IntervalValue::Inside(-1.0),
            Interval::<f64>::closed(-1.0, 2.0).capture(-1.0)
        );
        assert_eq!(
            IntervalValue::Inside(2.0),
            Interval::<f64>::closed(-1.0, 2.0).capture(2.0)
        );
        assert_eq!(
            IntervalValue::Below(-1.0),
            Interval::<f64>::open(-1.0, 2.0).capture(-1.0)
        );
        assert_eq!(
            IntervalValue::Above(2.0),
            Interval::<f64>::open(-1.0, 2.0).capture(2.0)
        );
        assert_eq!(
            IntervalValue::Below(-1.0),
            Interval::<f64>::closed(-1.0, 2.0).capture(-1.5)
        );
        assert_eq!(
            IntervalValue::Above(2.0),
            Interval::<f64>::closed(-1.0, 2.0).capture(2.5)
        );
        assert_eq!(
            IntervalValue::Below(-1.0),
            Interval::<f64>::open(-1.0, 2.0).capture(-1.5)
        );
        assert_eq!(
            IntervalValue::Above(2.0),
            Interval::<f64>::open(-1.0, 2.0).capture(2.5)
        );
    }
}
