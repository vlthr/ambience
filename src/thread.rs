use crate::error::*;
use std::any::Any;
use std::any::TypeId;
use std::cell::RefCell;
use std::{collections::HashMap, marker::PhantomData, sync::Arc};

thread_local! {
    static THREAD_LOCALS: RefCell<AmbientMap> = RefCell::new(AmbientMap::new());
}

/// Maps `TypeId`'s for stored ambient data to a stack of values for each type.
struct AmbientMap {
    data: HashMap<TypeId, Vec<Slot>>,
    /// Incremented each time a new value is pushed onto any stack.
    /// Used to give unique ID's to each allocation.
    id_counter: usize,
}

#[derive(Debug)]
struct Slot {
    ptr: Arc<dyn Any + 'static + Send + Sync>,
    id: usize,
}

impl AmbientMap {
    pub fn new() -> AmbientMap {
        AmbientMap {
            data: HashMap::new(),
            id_counter: 0,
        }
    }
    /// Returns a (cloned) Arc to the topmost (most recent still-active) value of type `T`.
    /// Returns `None` if no values are stored for the given `T`.
    pub fn peek<T: 'static + Send + Sync>(&self) -> Option<Arc<T>> {
        self.data.get(&TypeId::of::<T>()).and_then(|stack| {
            if stack.len() > 0 {
                stack[stack.len() - 1].ptr.clone().downcast::<T>().ok()
            } else {
                None
            }
        })
    }

    /// Pops the topmost (most recent still-active) value of type `T`.
    pub fn remove(&mut self, type_id: &TypeId, id: &usize) {
        self.data
            .get_mut(type_id)
            .expect("tried to remove empty ambient data stack")
            .retain(|slot| &slot.id != id);
    }
    /// Adds a new value of type `T` to the top of the stack.
    pub fn push<T: 'static + Send + Sync>(&mut self, new_val: Arc<T>) -> AmbientGuard<T> {
        let type_id = TypeId::of::<T>();
        let new_id = self.id_counter;
        self.id_counter += 1;
        self.data
            .entry(type_id)
            .or_insert_with(|| Vec::new())
            .push(Slot {
                ptr: new_val,
                id: new_id,
            });
        AmbientGuard {
            phantom: PhantomData::<*mut T>::default(),
            id: new_id,
        }
    }
}

/// Guard value for ambient data that was passed to `set()`. When `AmbientGuard` is dropped,
/// the value passed to `set()` is removed from the ambient data stack and is no longer accessible.
pub struct AmbientGuard<T>
where
    T: 'static,
{
    /// using `*mut T` ensures that this struct is `!Send`, which is necessary to prevent it
    /// from being sent to a different thread and becoming invalid.
    phantom: PhantomData<*mut T>,
    id: usize,
}

impl<T> Drop for AmbientGuard<T>
where
    T: 'static,
{
    fn drop(&mut self) {
        unset(&TypeId::of::<T>(), &self.id);
    }
}

/// Retrieves thread-local ambient data of type `T` that was previously set on the **same** thread.
/// The returned value will always be the most recently `set` value whose `AmbientGuard` still exists.
pub fn get<T: 'static + Send + Sync>() -> Result<Arc<T>> {
    THREAD_LOCALS.with(|frame_opt| {
        frame_opt
            .borrow()
            .peek::<T>()
            .ok_or_else(|| Error::ThreadAmbientUndefined(std::any::type_name::<T>()))
    })
}

/// TODO
fn unset(type_id: &TypeId, id: &usize) {
    THREAD_LOCALS.with(|frame_opt| {
        let mut storage = frame_opt.borrow_mut();
        storage.remove(type_id, id)
    })
}

/// Sets thread-local ambient data of type `T`. The value can be accessed on the **same**
/// thread until the returned `AmbientGuard` is dropped.
///
/// `set` can be called multiple times. The values of type `T` are treated as a stack, where
/// a call to `get::<T>()` will always yield the most recently added `T`. When the `AmbientGuard`
/// for a value on the stack is dropped, it is removed from the stack.
/// TODO: If the guard is manually dropped out-of-order, it is NOT guaranteed that the correct value is removed.
pub fn set<T: 'static + Send + Sync>(new_val: Arc<T>) -> AmbientGuard<T> {
    THREAD_LOCALS.with(|frame_opt| {
        let mut storage = frame_opt.borrow_mut();
        storage.push(new_val)
    })
}

#[cfg(test)]
mod tests {
    use crate::error::*;
    use crate::thread as ambience;
    use std::any::Any;
    use std::sync::Arc;

    #[test]
    fn simple() -> Result<()> {
        let one = Arc::new(1u64);
        {
            // set `one` as ambient data
            let _frame_guard = ambience::set(one.clone());
            assert_eq!(ambience::get::<u64>().unwrap(), one);
        }
        // no ambient frame in scope
        assert!(ambience::get::<u64>().is_err());
        Ok(())
    }

    #[test]
    fn nested() -> Result<()> {
        let one = Arc::new(1u64);
        let two = Arc::new(2u64);
        {
            // set `one` as ambient data
            let _frame_guard = ambience::set(one.clone());
            assert_eq!(ambience::get::<u64>().unwrap(), one);
            {
                // set `two` as ambient data in the inner scope
                let _frame_guard = ambience::set(two.clone());
                assert_eq!(ambience::get::<u64>().unwrap(), two);
            }
            // ambient data should be back to `one` after inner scope is dropped
            assert_eq!(ambience::get::<u64>().unwrap(), one);
        }
        // no ambient frame in scope
        assert!(ambience::get::<u64>().is_err());
        Ok(())
    }

    #[test]
    fn types_are_independent() -> Result<()> {
        let u_64 = Arc::new(64u64);
        let u_32 = Arc::new(32u32);
        {
            let _frame_guard = ambience::set(u_32.clone());
            let _frame_guard = ambience::set(u_64.clone());
            assert_eq!(ambience::get::<u64>().unwrap(), u_64);
            assert_eq!(ambience::get::<u32>().unwrap(), u_32);
        }
        // no ambient frame in scope
        assert!(ambience::get::<u64>().is_err());
        assert!(ambience::get::<u32>().is_err());
        Ok(())
    }

    #[test]
    fn manually_dropped() -> Result<()> {
        let one = Arc::new(1u64);
        let two = Arc::new(2u64);
        {
            // set `one` as ambient data
            let outer_frame_guard = ambience::set(one.clone());
            assert_eq!(ambience::get::<u64>().unwrap(), one);
            {
                // set `two` as ambient data in the inner scope
                let _frame_guard = ambience::set(two.clone());
                // dropping `outer_frame_guard` should remove `one` from the stack
                drop(outer_frame_guard);
                // `one` has been dropped, `two` should still be top of the stack
                assert_eq!(ambience::get::<u64>().unwrap(), two);
            }
            // `outer_frame_guard` was manually dropped, so after leaving the inner scope
            // there should be nothing left on the stack
            assert!(ambience::get::<u64>().is_err());
        }
        // no ambient frame in scope
        assert!(ambience::get::<u64>().is_err());
        Ok(())
    }

    #[test]
    fn downcast_expectations() -> Result<()> {
        let arc_any: Arc<dyn Any + 'static + Send + Sync> = Arc::new(1u64);
        assert!(arc_any.clone().downcast::<u64>().is_ok());
        assert!(arc_any.clone().downcast::<Arc<u64>>().is_err());
        Ok(())
    }

    #[test]
    /// ```compile_fail
    /// let one = 1u64;
    /// let one = Arc::new(&one);
    /// let outer_frame_guard = ambience::set(one.clone());
    /// ```
    fn data_must_be_static() {}

    #[test]
    /// ```compile_fail
    /// fn assert_send<T: Send>() {}
    ///
    /// assert_send<AmbientGuard<u64>>();
    /// ```
    fn guard_is_not_send() {}

    #[test]
    /// ```compile_fail
    /// fn assert_sync<T: Sync>() {}
    ///
    /// assert_sync<AmbientGuard<u64>>();
    /// ```
    fn guard_is_not_sync() {}
}