pub struct Null;

pub struct Undefined;

pub trait ShouldSkipSerializing {
    fn should_skip_serializing(&self) -> bool;
}
