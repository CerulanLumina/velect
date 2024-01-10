#![deny(rustdoc::broken_intra_doc_links)]
#![no_std]

//! # Velect: A Vec with item selection
//!
//! [`Velect`] is a wrapper around a [`Vec`] with selected item functionality -
//! which here means zero or one index from the vector is "selected" and can be
//! retrieved easily without needing to track the index of the selected item.
//!
//! ```
//! use velect::Velect;
//! // Creates a Velect with "three" selected.
//! let mut velect = Velect::from_vec(vec!["one", "two", "three", "four", "five"], Some(2));
//! assert_eq!(*velect.selected().unwrap(), "three");
//!
//! // Change the selection to "four"
//! velect.select_index(3);
//! assert_eq!(*velect.selected().unwrap(), "four");
//! ```
//!
//! Internally, velect tracks a selected index, which can be retrieved if desired, but
//! velect also updates this index to maintain the same logical selection whenever the
//! underlying vector is mutated:
//!
//! ```
//! use velect::Velect;
//! let mut velect = Velect::from_vec(vec!["one", "two", "three", "four", "five"], Some(2));
//! assert_eq!(*velect.selected().unwrap(), "three");
//! assert_eq!(velect.selected_index().unwrap(), 2);
//!
//! velect.remove(0);
//! assert_eq!(*velect.selected().unwrap(), "three");
//! assert_eq!(velect.selected_index().unwrap(), 1);
//! ```
//!
//! A [`Velect`] also implements `Deref<Target = Vec<_>>`, which allows any immutable functions
//! to be used transparently. Functions taking a mutable reference to self are re-implemented
//! with selection-preserving logic before a delegating to the underlying vector. It also implements
//! many of the same traits a regular `Vec` does, allowing it to be used in much the same way.

mod sliect;

extern crate alloc;
extern crate core;

use alloc::{
    borrow::{Cow, ToOwned},
    boxed::Box,
    rc::Rc,
    sync::Arc,
    vec::{Drain, Vec},
};
use core::{
    borrow::{Borrow, BorrowMut},
    cmp::Ordering,
    fmt::{Debug, Formatter},
    ops::{Bound, Deref, Index, IndexMut, RangeBounds},
    slice::SliceIndex,
};
use delegate::delegate;

pub use sliect::*;

// TODO: serde: Serialize, Deserialize

/// A wrapper around a [`Vec`] with selected item functionality.
///
/// The selection logic is implemented by a stored index, which is updated
/// whenever a mutating change is made to the underlying vector that necessitates it.
///
/// Use [`Velect::selected`] or [`Velect::selected_mut`] to retrieve a reference to the
/// selected item.
#[derive(Clone)]
pub struct Velect<T> {
    inner: Vec<T>,
    selected_index: Option<usize>,
}

impl<T> Velect<T> {
    /// Creates a new [`Velect`] with an empty underlying vector and no selection.
    pub fn new() -> Velect<T> {
        Self::from_vec(Vec::new(), None)
    }

    /// Creates a new [`Velect`] with a given underlying vector and an optional selection
    pub fn from_vec(v: Vec<T>, selected_index: Option<usize>) -> Velect<T> {
        assert!(
            selected_index.is_none() || selected_index.unwrap() < v.len(),
            "Selection index is out of bounds. Index: {}, Length: {}",
            selected_index.unwrap(),
            v.len()
        );
        Velect {
            inner: v,
            selected_index,
        }
    }

    /// Retrieves a reference to the selected element, or [`None`] if there is no selection.
    pub fn selected(&self) -> Option<&T> {
        self.selected_index.map(|a| &self.inner[a])
    }

    /// Retrieves a mutable reference to the selected element, or [`None`] if there is no selection.
    pub fn selected_mut(&mut self) -> Option<&mut T> {
        self.selected_index.map(|a| &mut self.inner[a])
    }

    /// Retrieves the currently selected index, or [`None`] if there is no selection.
    /// This may change after mutations and should not be relied upon. Use [`Velect::selected`]
    /// or [`Velect::selected_mut`] to retrieve a reference to selections.
    pub fn selected_index(&self) -> Option<usize> {
        self.selected_index
    }

    /// Select an index of the underlying vector.
    ///
    /// # Panics
    /// Panics when the index is out of bounds for the underlying vector.
    pub fn select_index(&mut self, index: usize) {
        assert!(
            index < self.inner.len(),
            "Selection index is out of bounds. Index: {}, Length: {}",
            index,
            self.inner.len()
        );
        self.selected_index = Some(index);
    }

    /// Sets the selected index. Calls [`Velect::select_index`] when some.
    ///
    /// # Panics
    /// Panics when the index is out of bounds for the underlying vector.
    pub fn select(&mut self, selected_index: Option<usize>) {
        match selected_index {
            Some(index) => self.select_index(index),
            None => self.selected_index = None,
        }
    }

    /// Deselects the selection (selection retrieval methods will return [`None`]).
    pub fn deselect(&mut self) {
        self.selected_index = None;
    }

    // Methods borrowing self as &mut that should not affect selection
    delegate! {
        to self.inner {
            /// See [`Vec::reserve`]
            pub fn reserve(&mut self, additional: usize); // Only affects capacity
            /// See [`Vec::reserve_exact`]
            pub fn reserve_exact(&mut self, additional: usize); // Only affects capacity
            /// See [`Vec::try_reserve`]
            pub fn try_reserve(&mut self, additional: usize) -> Result<(), alloc::collections::TryReserveError>; // Only affects capacity
            /// See [`Vec::try_reserve_exact`]
            pub fn try_reserve_exact(&mut self, additional: usize) -> Result<(), alloc::collections::TryReserveError>; // Only affects capacity
            /// See [`Vec::shrink_to_fit`]
            pub fn shrink_to_fit(&mut self); // Only affects capacity
            /// See [`Vec::shrink_to`]
            pub fn shrink_to(&mut self, min_capacity: usize); // Only affects capacity
            /// See [`Vec::push`]
            pub fn push(&mut self, value: T); // Only adds after all indices.
            /// See [`Vec::append`]
            pub fn append(&mut self, other: &mut Vec<T>); // Only adds after all indices.

            // Consumes self, selection info dropped
            /// See [`Vec::into_boxed_slice`]
            pub fn into_boxed_slice(self) -> Box<[T]>;

            // Elements in the slice can be altered but not added/removed.
            /// See [`Vec::as_mut_slice`]
            pub fn as_mut_slice(&mut self) -> &mut [T];
        }
    }

    /// Truncates the underlying `Vec` (see [`Vec::truncate`]). If the selected element is
    /// to be truncated, resets the selection to unselected / `None` and returns it as `Some(T)`.
    /// If not, `None` is returned.
    pub fn truncate(&mut self, len: usize) -> Option<T> {
        if len > self.inner.len() {
            return None;
        }
        if let Some(selected_index) = self.selected_index {
            if selected_index >= len {
                let ret = Some(self.inner.swap_remove(selected_index));
                self.selected_index = None;
                self.inner.truncate(len);
                ret
            } else {
                None
            }
        } else {
            self.inner.truncate(len);
            None
        }
    }

    /// Removes an element from the vector and returns it (see [`Vec::swap_remove`]).
    /// If the element was selected, resets the selection state to unselected / `None`..
    pub fn swap_remove(&mut self, index: usize) -> T {
        if let Some(selected_index) = self.selected_index {
            if selected_index == index {
                self.selected_index = None;
            } else if selected_index == self.inner.len() - 1 {
                self.selected_index = Some(index);
            }
        }
        self.inner.swap_remove(index)
    }

    /// Inserts an element into the underlying vector (see [`Vec::insert`]).
    /// Changes the currently selected index to maintain the same reference if necessary.
    pub fn insert(&mut self, index: usize, element: T) {
        if let Some(selected_index) = self.selected_index.as_mut() {
            if *selected_index >= index {
                *selected_index += 1;
            }
        }
        self.inner.insert(index, element);
    }

    /// Removes an element from the underlying vector (see [`Vec::remove`]).
    /// Changes the currently selected index to maintain the same reference if necessary.
    ///
    /// If the currently selected item's index is the index being removed, resets the
    /// selection to unselected / `None`.
    pub fn remove(&mut self, index: usize) -> T {
        if let Some(selected_index) = self.selected_index {
            match selected_index.cmp(&index) {
                Ordering::Greater => {
                    *self.selected_index.as_mut().unwrap() -= 1;
                }
                Ordering::Equal => {
                    self.selected_index = None;
                }
                Ordering::Less => {}
            }
        }
        self.inner.remove(index)
    }

    /// Clears the underlying vector (see [`Vec::clear`]).
    /// The selection is reset to unselected / `None`.
    pub fn clear(&mut self) {
        self.selected_index = None;
        self.inner.clear();
    }

    /// Pops an element from the underlying vector (see [`Vec::pop`]).
    /// The selection is reset to `None` if it was the last element.
    pub fn pop(&mut self) -> Option<T> {
        if let Some(selected_index) = self.selected_index {
            if selected_index == self.len() - 1 {
                self.selected_index = None;
            }
        }
        self.inner.pop()
    }

    /// Drains items (see [`Vec::drain`]) from the underlying vector.
    /// The selection is reset to unselected / `None` if the range contains the selection.
    ///
    /// The selection index may change since [`Vec::drain`] may move elements
    /// (similar to [`Vec::swap_remove`]).
    pub fn drain<R: RangeBounds<usize>>(&mut self, range: R) -> Drain<'_, T> {
        if let Some(selected_index) = &self.selected_index {
            if range.contains(selected_index) {
                self.selected_index = None;
            } else {
                let start_index = match range.start_bound() {
                    Bound::Included(index) => *index,
                    Bound::Excluded(index) => *index + 1,
                    Bound::Unbounded => 0,
                };
                let end_index = match range.end_bound() {
                    Bound::Included(index) => *index,
                    Bound::Excluded(index) => *index - 1,
                    Bound::Unbounded => self.inner.len() - 1,
                };
                if *selected_index > start_index {
                    // We will be in the remaining tail
                    let new_index = *selected_index - end_index + start_index - 1;
                    self.selected_index = Some(new_index);
                }
            }
        }
        self.inner.drain(range)
    }

    /// Resizes the underlying vector (see [`Vec::resize_with`]). If the selection is to be
    /// truncated, resets it to unselected / `None`.
    pub fn resize_with<F>(&mut self, new_len: usize, f: F) -> Option<T>
    where
        F: FnMut() -> T,
    {
        let ret = if let Some(selected_index) = self.selected_index {
            if selected_index >= new_len {
                self.selected_index = None;
                Some(self.inner.swap_remove(selected_index))
            } else {
                None
            }
        } else {
            None
        };
        self.inner.resize_with(new_len, f);
        ret
    }

    // Not doing
    // as_mut_ptr
    // set_len
    // split_off

    // Not doing... yet...
    // retain
    // retain_mut
    // dedup_by_key
    // dedup_by
}

impl<T> Velect<T>
where
    T: Clone,
{
    delegate! {
        to self.inner {
            pub fn extend_from_slice(&mut self, other: &[T]);
            pub fn extend_from_within<R>(&mut self, src: R) where R: core::ops::RangeBounds<usize>;
        }
    }

    /// Resizes the underlying vector (see [`Vec::resize`]). If the selection is to be
    /// truncated, resets it to unselected / `None`.
    pub fn resize(&mut self, new_len: usize, value: T) -> Option<T> {
        self.resize_with(new_len, || value.clone())
    }
}

impl<T> Default for Velect<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Deref for Velect<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T, I: SliceIndex<[T]>> Index<I> for Velect<T> {
    type Output = <I as SliceIndex<[T]>>::Output;

    delegate! {
        to self.inner {
            fn index(&self, index: I) -> &Self::Output;
        }
    }
}

impl<T, I: SliceIndex<[T]>> IndexMut<I> for Velect<T> {
    delegate! {
        to self.inner {
            fn index_mut(&mut self, index: I) -> &mut Self::Output;
        }
    }
}

impl<T> AsMut<[T]> for Velect<T> {
    delegate! {
        to self.inner {
            fn as_mut(&mut self) -> &mut [T];
        }
    }
}

impl<T> AsRef<[T]> for Velect<T> {
    delegate! {
        to self.inner {
            fn as_ref(&self) -> &[T];
        }
    }
}

impl<T> BorrowMut<[T]> for Velect<T> {
    delegate! {
        to self.inner {
            fn borrow_mut(&mut self) -> &mut [T];
        }
    }
}

impl<T> Borrow<[T]> for Velect<T> {
    delegate! {
        to self.inner {
            fn borrow(&self) -> &[T];
        }
    }
}

impl<'a, T> IntoIterator for &'a Velect<T> {
    type Item = &'a T;
    type IntoIter = alloc::slice::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut Velect<T> {
    type Item = &'a mut T;
    type IntoIter = alloc::slice::IterMut<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter_mut()
    }
}

impl<T> IntoIterator for Velect<T> {
    type Item = T;
    type IntoIter = alloc::vec::IntoIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<'a, T: Copy + 'a> Extend<&'a T> for Velect<T> {
    delegate! {
        to self.inner {
            fn extend<I: IntoIterator<Item = &'a T>>(&mut self, iter: I);
        }
    }
}

impl<T> Extend<T> for Velect<T> {
    delegate! {
        to self.inner {
            fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I);
        }
    }
}

impl<'a, T: Clone> From<&'a Velect<T>> for Cow<'a, [T]> {
    fn from(v: &'a Velect<T>) -> Cow<'a, [T]> {
        (&v.inner).into()
    }
}

impl<T: Clone, const N: usize> From<&[T; N]> for Velect<T> {
    fn from(s: &[T; N]) -> Self {
        Self {
            inner: s.into(),
            selected_index: None,
        }
    }
}

impl<T: Clone> From<&[T]> for Velect<T> {
    fn from(s: &[T]) -> Self {
        Self {
            inner: s.into(),
            selected_index: None,
        }
    }
}

impl<T: Clone, const N: usize> From<&mut [T; N]> for Velect<T> {
    fn from(s: &mut [T; N]) -> Self {
        Self {
            inner: s.into(),
            selected_index: None,
        }
    }
}

impl<T: Clone> From<&mut [T]> for Velect<T> {
    fn from(s: &mut [T]) -> Self {
        Self {
            inner: s.into(),
            selected_index: None,
        }
    }
}

impl<T> From<Box<[T]>> for Velect<T> {
    fn from(s: Box<[T]>) -> Self {
        Self {
            inner: s.into(),
            selected_index: None,
        }
    }
}

impl<'a, T> From<Cow<'a, [T]>> for Velect<T>
where
    [T]: ToOwned<Owned = Vec<T>>,
{
    fn from(s: Cow<'a, [T]>) -> Self {
        Self {
            inner: s.into(),
            selected_index: None,
        }
    }
}

macro_rules! from_owned_inner {
    ($($t: ty),*) => {
        $(
        impl<T> From<Velect<T>> for $t {
            fn from(v: Velect<T>) -> $t {
                v.inner.into()
            }
        }
        )*
    };
}

from_owned_inner!(Arc<[T]>, Box<[T]>, Rc<[T]>);

impl<'a, T: Clone> From<Velect<T>> for Cow<'a, [T]> {
    fn from(value: Velect<T>) -> Self {
        Cow::Owned(value.inner)
    }
}

impl<T, const N: usize> From<[T; N]> for Velect<T> {
    fn from(value: [T; N]) -> Self {
        Self {
            inner: value.into(),
            selected_index: None,
        }
    }
}

impl<T> FromIterator<T> for Velect<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self {
            inner: Vec::from_iter(iter),
            selected_index: None,
        }
    }
}

impl<T: Debug> Debug for Velect<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "Velect{{ selected_index: {:?}, inner: {:?} }}",
            self.selected_index, self.inner
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::Velect;
    use alloc::vec;

    fn create_velect() -> Velect<&'static str> {
        let vec = vec![
            "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
        ];
        Velect::from_vec(vec, None)
    }

    mod truncate {
        use super::*;
        #[test]
        fn truncate_without_selection() {
            let mut velect = create_velect();
            velect.truncate(5);
            assert!(
                velect.selected().is_none(),
                "Velect somehow got a selection from None"
            )
        }

        #[test]
        fn truncate_with_unaffected_selection() {
            let mut velect = create_velect();
            velect.select_index(4); // "five"
            let ret = velect.truncate(5);
            assert!(
                ret.is_none(),
                "Velect returned on a truncate without affected the selected index"
            );
            assert_eq!(
                *velect.selected().unwrap(),
                "five",
                "Velect changed an index during truncation which should be unaffected"
            );
        }

        #[test]
        fn truncate_with_affected_selection() {
            let mut velect = create_velect();
            velect.select_index(5);
            let ret = velect.truncate(5);
            assert!(ret.is_some(), "Velect did not return a truncated selection");
            assert_eq!(ret.unwrap(), "six", "Velect did not return the right item");
            assert!(
                velect.selected().is_none(),
                "Velect did not reset the selected index"
            );
        }
    }

    mod swap_remove {
        use super::*;
        #[test]
        fn swap_remove_unaffected() {
            let mut velect = create_velect();
            velect.select_index(1); // "two"
            velect.swap_remove(0);
            velect.swap_remove(2);
            assert!(
                velect.selected().is_some(),
                "Selection should not be affected"
            );
            assert_eq!(
                *velect.selected().unwrap(),
                "two",
                "Selected item should be unaffected"
            );
            assert_eq!(velect.len(), 8, "Items should have been removed");
            assert_eq!(velect[0], "ten", "swap_remove did not execute correctly.");
            assert_eq!(velect[1], "two", "swap_remove did not execute correctly.");
            assert_eq!(velect[2], "nine", "swap_remove did not execute correctly.");
        }

        #[test]
        fn swap_remove_affected_by_removal() {
            let mut velect = create_velect();
            velect.select_index(4); // "five"
            assert_eq!(velect.swap_remove(4), "five");
            assert!(
                velect.selected().is_none(),
                "swap_remove should leave no selection"
            );
        }

        #[test]
        fn swap_remove_affected_by_last() {
            let mut velect = create_velect();
            velect.select_index(velect.len() - 1);
            assert_eq!(
                *velect.selected().unwrap(),
                "ten",
                "Selected item should be unaffected"
            );
            velect.swap_remove(0);
            assert_eq!(
                *velect.selected().unwrap(),
                "ten",
                "Selected item should be unaffected"
            );
        }
    }

    mod insert {
        use super::*;

        #[test]
        fn insert_unaffected_index_before() {
            let mut velect = create_velect();
            velect.select_index(4); // "five"
            velect.insert(5, "five point five");
            assert!(velect.selected().is_some(), "Selection should remain valid");
            assert_eq!(
                *velect.selected().unwrap(),
                "five",
                "insert should leave selection unaffected"
            );
        }

        #[test]
        fn insert_affected_index_at() {
            let mut velect = create_velect();
            velect.select_index(4); // "five"
            velect.insert(4, "four point five");
            assert!(velect.selected().is_some(), "Selection should remain valid");
            assert_eq!(
                *velect.selected().unwrap(),
                "five",
                "insert should appropriately affect index"
            );
        }

        #[test]
        fn insert_affected_index_after() {
            let mut velect = create_velect();
            velect.select_index(6); // "seven"
            velect.insert(4, "four point five");
            assert!(velect.selected().is_some(), "Selection should remain valid");
            assert_eq!(
                *velect.selected().unwrap(),
                "seven",
                "insert should appropriately affect index"
            );
        }
    }

    mod remove {
        use super::*;

        #[test]
        fn remove_selection_unaffected_after() {
            let mut velect = create_velect();
            velect.select_index(6); // "seven";
            assert_eq!(velect.remove(8), "nine");
            assert_eq!(velect.remove(8), "ten");
            assert_eq!(velect.remove(7), "eight");
            assert!(velect.selected().is_some(), "Selection should remain valid");
            assert_eq!(
                *velect.selected().unwrap(),
                "seven",
                "remove should leave selection unaffected"
            );
        }

        #[test]
        fn remove_selection_affected_before() {
            let mut velect = create_velect();
            velect.select_index(6); // "seven";

            assert_eq!(velect.remove(0), "one");
            assert!(velect.selected().is_some(), "Selection should remain valid");
            assert_eq!(
                *velect.selected().unwrap(),
                "seven",
                "remove should not affect index when unnecessary"
            );

            assert_eq!(velect.remove(0), "two");
            assert!(velect.selected().is_some(), "Selection should remain valid");
            assert_eq!(
                *velect.selected().unwrap(),
                "seven",
                "remove should not affect index when unnecessary"
            );

            assert_eq!(velect.remove(1), "four");
            assert!(velect.selected().is_some(), "Selection should remain valid");
            assert_eq!(
                *velect.selected().unwrap(),
                "seven",
                "remove should not affect index when unnecessary"
            );
        }

        #[test]
        fn remove_selection_at() {
            let mut velect = create_velect();
            velect.select_index(6); // "seven";

            assert_eq!(velect.remove(6), "seven");
            assert!(
                velect.selected().is_none(),
                "Removing selection should reset selection to None"
            );
        }

        #[test]
        fn remove_selection_at_multi() {
            let mut velect = create_velect();
            velect.select_index(6); // "seven";

            assert_eq!(velect.remove(0), "one");
            assert_eq!(
                *velect.selected().unwrap(),
                "seven",
                "selection should remain valid"
            );

            assert_eq!(velect.remove(0), "two");
            assert_eq!(
                *velect.selected().unwrap(),
                "seven",
                "selection should remain valid"
            );

            assert_eq!(velect.remove(0), "three");
            assert_eq!(
                *velect.selected().unwrap(),
                "seven",
                "selection should remain valid"
            );

            assert_eq!(velect.remove(0), "four");
            assert_eq!(
                *velect.selected().unwrap(),
                "seven",
                "selection should remain valid"
            );

            assert_eq!(velect.remove(0), "five");
            assert_eq!(
                *velect.selected().unwrap(),
                "seven",
                "selection should remain valid"
            );

            assert_eq!(velect.remove(0), "six");
            assert_eq!(
                *velect.selected().unwrap(),
                "seven",
                "selection should remain valid"
            );

            assert_eq!(velect.remove(0), "seven");
            assert!(
                velect.selected().is_none(),
                "Removing selection should reset selection to None"
            );
        }
    }

    mod clear {
        use super::*;

        #[test]
        fn clear_unsets_selection() {
            let mut velect = create_velect();
            velect.select_index(2); // three;
            assert!(velect.selected().is_some());
            assert_eq!(
                *velect.selected().unwrap(),
                "three",
                "selection is not valid"
            );

            velect.clear();
            assert!(velect.is_empty(), "clear did not clear");
            assert!(velect.selected().is_none(), "clear did not reset selection");
        }
    }

    mod pop {
        use super::*;

        #[test]
        fn pop_unaffected_if_not_last() {
            let mut velect = create_velect();
            velect.select_index(8); // "nine"
            assert!(velect.selected().is_some());
            assert_eq!(
                *velect.selected().unwrap(),
                "nine",
                "selection is not valid"
            );

            assert_eq!(
                velect.pop().unwrap(),
                "ten",
                "pop did not remove the correct element"
            );

            assert!(
                velect.selected().is_some(),
                "pop should not affect non-last selection"
            );
            assert_eq!(
                *velect.selected().unwrap(),
                "nine",
                "pop should not affect non-last selection"
            );
        }

        #[test]
        fn pop_affected_if_last() {
            let mut velect = create_velect();
            velect.select_index(8); // "nine"
            assert!(velect.selected().is_some());
            assert_eq!(
                *velect.selected().unwrap(),
                "nine",
                "selection is not valid"
            );

            assert_eq!(
                velect.pop().unwrap(),
                "ten",
                "pop did not remove the correct element"
            );
            assert!(
                velect.selected().is_some(),
                "pop should not affect non-last selection"
            );
            assert_eq!(
                *velect.selected().unwrap(),
                "nine",
                "pop should not affect non-last selection"
            );

            assert_eq!(
                velect.pop().unwrap(),
                "nine",
                "pop did not remove the correct element"
            );
            assert!(velect.selected().is_none(), "pop should reset selection");
        }

        #[test]
        fn pop_empty() {
            let mut velect = Velect::<&'static str>::new();
            assert!(velect.selected().is_none());
            assert!(velect.pop().is_none());
            assert!(velect.selected().is_none());
        }
    }

    mod drain {
        use super::*;

        #[test]
        fn drain_unaffected_outside_range_before() {
            let mut velect = create_velect();
            velect.select_index(5); // "six"
            assert!(velect.selected().is_some());
            assert_eq!(*velect.selected().unwrap(), "six", "selection is not valid");

            {
                velect.drain(6..);
            }

            assert!(
                velect.selected().is_some(),
                "drain should not affect selection outside range"
            );
            assert_eq!(
                *velect.selected().unwrap(),
                "six",
                "drain should not affect selection outside range"
            );
        }

        #[test]
        fn drain_unaffected_outside_range_after() {
            let mut velect = create_velect();
            velect.select_index(5); // "six"
            assert!(velect.selected().is_some());
            assert_eq!(*velect.selected().unwrap(), "six", "selection is not valid");

            {
                velect.drain(..5);
            }

            assert!(
                velect.selected().is_some(),
                "drain should not affect selection outside range"
            );
            assert_eq!(
                *velect.selected().unwrap(),
                "six",
                "drain should not affect selection outside range"
            );
        }

        #[test]
        fn drain_unaffected_outside_range_unbounded_start() {
            let mut velect = create_velect();
            velect.select_index(9); // "ten"
            assert!(velect.selected().is_some());
            assert_eq!(*velect.selected().unwrap(), "ten", "selection is not valid");

            {
                velect.drain(..5);
            }

            assert!(
                velect.selected().is_some(),
                "drain should not affect selection outside range"
            );
            assert_eq!(
                *velect.selected().unwrap(),
                "ten",
                "drain should not affect selection outside range"
            );
        }

        #[test]
        fn drain_unaffected_outside_range_inclusive_end() {
            let mut velect = create_velect();
            velect.select_index(9); // "ten"
            assert!(velect.selected().is_some());
            assert_eq!(*velect.selected().unwrap(), "ten", "selection is not valid");

            {
                velect.drain(..=8);
            }

            assert!(
                velect.selected().is_some(),
                "drain should not affect selection outside range"
            );
            assert_eq!(
                *velect.selected().unwrap(),
                "ten",
                "drain should not affect selection outside range"
            );
        }

        #[test]
        fn drain_unaffected_outside_range_exclusive_end() {
            let mut velect = create_velect();
            velect.select_index(9); // "ten"
            assert!(velect.selected().is_some());
            assert_eq!(*velect.selected().unwrap(), "ten", "selection is not valid");

            {
                velect.drain(4..5);
            }

            assert!(
                velect.selected().is_some(),
                "drain should not affect selection outside range"
            );
            assert_eq!(
                *velect.selected().unwrap(),
                "ten",
                "drain should not affect selection outside range"
            );
        }

        #[test]
        fn drain_unaffected_outside_range_excluded_at_index() {
            let mut velect = create_velect();
            velect.select_index(9); // "ten"
            assert!(velect.selected().is_some());
            assert_eq!(*velect.selected().unwrap(), "ten", "selection is not valid");

            {
                velect.drain(4..9);
            }

            assert!(
                velect.selected().is_some(),
                "drain should not affect selection outside range"
            );
            assert_eq!(
                *velect.selected().unwrap(),
                "ten",
                "drain should not affect selection outside range"
            );
        }

        #[test]
        fn drain_affected_inside_range() {
            let mut velect = create_velect();
            velect.select_index(5); // "six"
            assert!(velect.selected().is_some());
            assert_eq!(*velect.selected().unwrap(), "six", "selection is not valid");

            {
                velect.drain(3..6);
            }

            assert!(
                velect.selected().is_none(),
                "drain should reset selection within range"
            );
        }
    }

    mod resize {
        use super::*;

        #[test]
        fn resize_as_extend() {
            let mut velect = create_velect();
            velect.select_index(8); // "nine"
            assert!(velect.selected().is_some());
            assert_eq!(
                *velect.selected().unwrap(),
                "nine",
                "selection is not valid"
            );

            velect.resize(20, "more");

            assert!(velect.selected().is_some());
            assert_eq!(
                *velect.selected().unwrap(),
                "nine",
                "selection is not valid"
            );
            assert_eq!(
                velect.selected_index().unwrap(),
                8,
                "resize (extend) should not change selected index"
            );
        }

        #[test]
        fn resize_as_truncate_selection_truncated() {
            let mut velect = create_velect();
            velect.select_index(8); // "nine"
            assert!(velect.selected().is_some());
            assert_eq!(
                *velect.selected().unwrap(),
                "nine",
                "selection is not valid"
            );

            velect.resize(8, "less");

            assert!(
                velect.selected().is_none(),
                "resize (truncate) should deselect when less"
            );
        }

        #[test]
        fn resize_as_truncate_selection_less_than_newlen() {
            let mut velect = create_velect();
            velect.select_index(8); // "nine"
            assert!(velect.selected().is_some());
            assert_eq!(
                *velect.selected().unwrap(),
                "nine",
                "selection is not valid"
            );

            velect.resize(9, "less");

            assert!(velect.selected().is_some());
            assert_eq!(
                *velect.selected().unwrap(),
                "nine",
                "selection is not valid"
            );
            assert_eq!(
                velect.selected_index().unwrap(),
                8,
                "resize (truncate) should not change selected index when selection is still valid"
            );
        }
    }

    mod resize_with {
        use super::*;

        #[test]
        fn resize_with_as_extend() {
            let mut velect = create_velect();
            velect.select_index(8); // "nine"
            assert!(velect.selected().is_some());
            assert_eq!(
                *velect.selected().unwrap(),
                "nine",
                "selection is not valid"
            );

            velect.resize_with(20, || "more");

            assert!(velect.selected().is_some());
            assert_eq!(
                *velect.selected().unwrap(),
                "nine",
                "selection is not valid"
            );
            assert_eq!(
                velect.selected_index().unwrap(),
                8,
                "resize_with (extend) should not change selected index"
            );
        }

        #[test]
        fn resize_with_as_truncate_selection_truncated() {
            let mut velect = create_velect();
            velect.select_index(8); // "nine"
            assert!(velect.selected().is_some());
            assert_eq!(
                *velect.selected().unwrap(),
                "nine",
                "selection is not valid"
            );

            velect.resize_with(8, || "less");

            assert!(
                velect.selected().is_none(),
                "resize_with (truncate) should deselect when less"
            );
        }

        #[test]
        fn resize_with_as_truncate_selection_less_than_newlen() {
            let mut velect = create_velect();
            velect.select_index(8); // "nine"
            assert!(velect.selected().is_some());
            assert_eq!(
                *velect.selected().unwrap(),
                "nine",
                "selection is not valid"
            );

            velect.resize_with(9, || "less");

            assert!(velect.selected().is_some());
            assert_eq!(
                *velect.selected().unwrap(),
                "nine",
                "selection is not valid"
            );
            assert_eq!(velect.selected_index().unwrap(), 8, "resize_with (truncate) should not change selected index when selection is still valid");
        }
    }
}
