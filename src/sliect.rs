/// A slice with selection functionality.
///
/// For use with mutable slices, see [`SliectMut`]
pub struct Sliect<'a, T> {
    pub inner: &'a [T],
    pub selected_index: Option<usize>,
}

/// A mutable slice with selection functionality
///
/// For use with immutable slices see [`Sliect`]
pub struct SliectMut<'a, T> {
    pub inner: &'a mut [T],
    pub selected_index: Option<usize>,
}

/// A slice with selection functionality. In contrast to [`Sliect`],
/// exactly one index is always selected, rather than 0 or 1.
///
/// For use with mutable slices see [`SliectDefiniteMut`]
pub struct SliectDefinite<'a, T> {
    pub inner: &'a [T],
    pub selected_index: usize,
}

/// A mutable slice with selection functionality. In contrast to [`SliectMut`],
/// exactly one index is always selected, rather than 0 or 1.
///
/// For use with immutable slices see [`SliectDefinite`]
pub struct SliectDefiniteMut<'a, T> {
    pub inner: &'a mut [T],
    pub selected_index: usize,
}

impl<'a, T> Sliect<'a, T> {
    /// Creates a Sliect from an immutable slice and an optional initial selection
    pub fn new_from_slice(inner: &'a [T], selection: Option<usize>) -> Self {
        Self {
            inner,
            selected_index: selection,
        }
    }

    /// Retrieves the selected item from the slice
    pub fn selected(&self) -> Option<&'a T> {
        self.selected_index.map(|a| &self.inner[a])
    }

    /// Retrieves the currently selected index
    pub fn selected_index(&self) -> Option<usize> {
        self.selected_index
    }

    /// Sets the currently selected index
    ///
    /// # Panics
    /// Panics when the index is out of bounds for the underlying slice.
    pub fn select_index(&mut self, index: Option<usize>) {
        match index {
            Some(index) => {
                let len = self.inner.len();
                if index < len {
                    self.selected_index = Some(index);
                } else {
                    panic!("Index is out of bounds for Sliect: {index} >= {len}");
                }
            }
            None => {
                self.selected_index = None;
            }
        }
    }
}

impl<'a, T> SliectMut<'a, T> {
    /// Creates a Sliect from an immutable slice and an optional initial selection
    pub fn new_from_slice(inner: &'a mut [T], selected_index: Option<usize>) -> Self {
        Self {
            inner,
            selected_index,
        }
    }

    /// Retrieves a reference to the selected item from the slice
    pub fn selected(&'a self) -> Option<&'a T> {
        self.selected_index.map(|a| &self.inner[a])
    }

    /// Retrieves a mutable reference to the selected item from the slice
    pub fn selected_mut(&'a mut self) -> Option<&'a mut T> {
        self.selected_index.map(|a| &mut self.inner[a])
    }

    /// Retrieves the currently selected index
    pub fn selected_index(&self) -> Option<usize> {
        self.selected_index
    }

    /// Sets the currently selected index
    ///
    /// # Panics
    /// Panics when the index is out of bounds for the underlying slice..
    pub fn select_index(&mut self, index: Option<usize>) {
        match index {
            Some(index) => {
                let len = self.inner.len();
                if index < len {
                    self.selected_index = Some(index);
                } else {
                    panic!("Index is out of bounds for SliectMut: {index} >= {len}");
                }
            }
            None => {
                self.selected_index = None;
            }
        }
    }
}

impl<'a, T> SliectDefinite<'a, T> {
    /// Creates a Sliect from an immutable slice and an initial selection
    pub fn new_from_slice(inner: &'a [T], selection: usize) -> Self {
        Self {
            inner,
            selected_index: selection,
        }
    }

    /// Retrieves the selected item from the slice
    pub fn selected(&self) -> &'a T {
        &self.inner[self.selected_index]
    }

    /// Retrieves the currently selected index
    pub fn selected_index(&self) -> usize {
        self.selected_index
    }

    /// Sets the currently selected index
    ///
    /// # Panics
    /// Panics when the index is out of bounds for the underlying slice.
    pub fn select_index(&mut self, index: usize) {
        let len = self.inner.len();
        if index < len {
            self.selected_index = index;
        } else {
            panic!("Index is out of bounds for SliectDefinite: {index} >= {len}");
        }
    }
}

impl<'a, T> SliectDefiniteMut<'a, T> {
    /// Creates a Sliect from an immutable slice and an optional initial selection
    pub fn new_from_slice(inner: &'a mut [T], selected_index: usize) -> Self {
        Self {
            inner,
            selected_index,
        }
    }

    /// Retrieves a reference to the selected item from the slice
    pub fn selected(&'a self) -> &'a T {
        &self.inner[self.selected_index]
    }

    /// Retrieves a mutable reference to the selected item from the slice
    pub fn selected_mut(&'a mut self) -> &'a mut T {
        &mut self.inner[self.selected_index]
    }

    /// Retrieves the currently selected index
    pub fn selected_index(&self) -> usize {
        self.selected_index
    }

    /// Sets the currently selected index
    ///
    /// # Panics
    /// Panics when the index is out of bounds for the underlying slice.
    pub fn select_index(&mut self, index: usize) {
        let len = self.inner.len();
        if index < len {
            self.selected_index = index;
        } else {
            panic!("Index is out of bounds for SliectDefiniteMut: {index} >= {len}");
        }
    }
}
