use bumpalo::Bump;

/// Thin wrapper around [`bumpalo::Bump`] for arena allocation.
pub struct Arena {
    bump: Bump,
}

impl Arena {
    pub fn new() -> Self {
        Self { bump: Bump::new() }
    }

    pub fn alloc<T>(&self, val: T) -> &T {
        self.bump.alloc(val)
    }

    pub fn alloc_slice_copy<T: Copy>(&self, src: &[T]) -> &[T] {
        self.bump.alloc_slice_copy(src)
    }

    pub fn reset(&mut self) {
        self.bump.reset();
    }
}

impl Default for Arena {
    fn default() -> Self {
        Self::new()
    }
}
