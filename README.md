# trio

I wrote this because John De Goes nerd sniped me :)

This is like the `RIO` monad: Reader + IO. In `RIO`, all exceptions
are unchecked exceptions. There are valid reasons for this, based on
how the GHC runtime system handles exceptions and async exceptions.

For scalaz, John recently discussed the idea of `IO` being a
bifunctor: one type parameter for the types of exceptions, the other
for the result. In discussions at LambdaConf Winter Retreat 2018, we
discussed whether this idea could apply to Haskell. I pointed out that
it couldn't, because `IO` in Haskell means "it can throw any exception
type it wants." But John made an argument that I couldn't shake:
perhaps we could make things work. This library is an experiment in
that direction.

__NOTE__ I just said _experiment_. This is nothing more than an
experiment. I'm still using `RIO` and recommending it for production
code. This is pure exploration.

As said, `IO` is scalaz will become a bifunctor. `RIO` is a
profunctor: the `r` environment variable is contravariant, and the
result value covariant. `Trio` is a trifunctor, with the reader being
contravariant, and the exception and result being covariant. So we have:

```haskell
newtype Trio r e a
```

The semantics of this are as follows: the type of `e` is the type of
_checked exceptions_ inside the action. These are actions that are
intended to be caught. However, _other exceptions may occur_. In
particular:

* As is always the case in Haskell code, an asynchronous exception can
  occur anywhere.
* By using the `throwUnchecked` function

Under the surface, this library is using unsafe shenanigans to make
things work. That is theoretically hidden away entirely by hiding the
internal interface.

## `From` typeclasses

Exceptions are intended to have helper `From` typeclasses for
conversions. For example:

```haskell
class FromIOException e where
  fromIOException :: E.IOException -> e

instance FromIOException E.IOException where
  fromIOException = id
instance FromIOException E.SomeException where
  fromIOException = E.toException
```

This allows us to more easily coalesce different exception types. But
more importantly that convenience, it allows us to write more useful
`with`-style functions, e.g.:

```haskell
withBinaryFile
  :: FromIOException e
  => FilePath
  -> IO.IOMode
  -> (IO.Handle -> Trio r e a)
  -> Trio r e a
```

Notice how the inner function is free to dictate a different exception
type if desired.

__NOTE__ It may make more sense to use a lensy `Prism` here instead.

## Catch vs cleanup

Like the safe-exceptions library, this library is designed around
proper handling of async exceptions. To make this clear, we
distinguish between two cases of exception handling:

* Catch-and-recover
* Cleanup-and-rethrow

The former applies to functions like `catch` and `try`, which prevent
exception propagation. Those apply exclusively here to the checked
exception type (the second parameter to `Trio`).

The latter applies to _all_ exception types: checked, unchecked, and
asynchronous. These are functions like `bracket`, `finally`, and
`onException`, which will rethrow the generated exception.

## Usage of Void

Like the scalaz implementation, the idea here is to indicate that all
exceptions have been handled by placing a `Void` value as the checked
exception type.

## Example usage

For now, just check out the test suite.
