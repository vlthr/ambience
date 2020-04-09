use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("no thread-local ambient value of type `{0}` exists")]
    ThreadAmbientUndefined(&'static str),
    #[error("unknown error")]
    Unknown,
}
