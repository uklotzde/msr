pub mod interval;

use self::interval::Interval;

use std::ops::Bound;

/// Linearly projects a 1-dimensional `point` (= number) from a
/// non-empty and bounded `source` interval into a non-empty and
/// bounded `target` interval. Returns `None` if the result is
/// undefined or ambiguous.
#[cfg_attr(feature = "cargo-clippy", allow(float_cmp))]
pub fn project_linear_1d(point: f64, source: Interval<f64>, target: Interval<f64>) -> Option<f64> {
    if source.is_empty() || target.is_empty() {
        return None;
    }
    let source_min = match source.lower_bound() {
        Bound::Included(lower) | Bound::Excluded(lower) => *lower,
        Bound::Unbounded => return None,
    };
    let source_max = match source.upper_bound() {
        Bound::Included(upper) | Bound::Excluded(upper) => *upper,
        Bound::Unbounded => return None,
    };
    debug_assert!(source.is_bounded());
    debug_assert!(source_min <= source_max);
    let target_min = match target.lower_bound() {
        Bound::Included(lower) | Bound::Excluded(lower) => *lower,
        Bound::Unbounded => return None,
    };
    let target_max = match target.upper_bound() {
        Bound::Included(upper) | Bound::Excluded(upper) => *upper,
        Bound::Unbounded => return None,
    };
    debug_assert!(target.is_bounded());
    debug_assert!(target_min <= target_max);
    if source_min < source_max {
        let s = (target_max - target_min) / (source_max - source_min);
        Some((point - source_min) * s + target_min)
    } else if source_min == source_max && target_min == target_max && point == source_min {
        // Edge case: 2 singleton intervals and `source` contains `point`
        debug_assert!(source == Interval::singleton(source_min));
        debug_assert!(source.contains(point));
        debug_assert!(target == Interval::singleton(target_min));
        debug_assert!(target.contains(target_min));
        debug_assert!(point == source_min);
        Some(target_min)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn project_linear_1d() {
        assert_eq!(
            super::project_linear_1d(0.0, Interval::closed(4.0, 20.0), Interval::open(0.0, 1.0)),
            Some(-0.25)
        );
        assert_eq!(
            super::project_linear_1d(20.0, Interval::open(4.0, 20.0), Interval::closed(0.16, 3.2)),
            Some(3.2)
        );
        assert_eq!(
            super::project_linear_1d(1.0, Interval::singleton(1.0), Interval::singleton(2.0)),
            Some(2.0)
        );
        assert_eq!(
            super::project_linear_1d(1.5, Interval::singleton(1.0), Interval::singleton(2.0)),
            None
        );
    }
}
