# contra-tracer

Logging and monitoring should be treated as a software feature and not just a
debugging tool.
Yet it's common to find logging definitions which use monads and typeclasses
to do what is essentially printf debugging: the log items are big types like
text or JSON, and these items may be logged any where, any time, because the
logging implementation is part of some omnipresent application monad.

This package provides a minimal set of definitions for _contravariant tracing_.
It is intended to express the pattern--found in logging and monitoring--in which
domain-specific values (e.g. events, statistics) are provided to
domain-agnostic processors (e.g. syslog). A program which has a `Tracer m t` in
scope is able to log any `t` with side-effects in `m`. By choosing `t` to be as
small as possible, the program becomes more modular, because a tracer on a
bigger type is also a tracer on a smaller type:

```Haskell
-- This is from Data.Functor.Contravariant
-- (t -> s) shows that s is bigger than t
contramap :: (t -> s) -> Tracer m s -> Tracer m t
```

The `Contravariant` instance on `Tracer m` is the mechanism by which a
domain-agnostic tracer is adapted to stand in where a domain-specific
tracer is required. This is why we call it contravariant tracing.

```Haskell
-- Puts text to stdout.
stdoutTracer :: Tracer IO Text

-- A domain-specific event type.
data Event = EventA | EventB Int

-- The log format for an Event.
eventToText :: Event -> Text

-- The stdoutTracer becomes an Event tracer by way of the Contravariant
-- instance.
eventTracer :: Tracer IO Event
eventTracer = contramap eventToText stdoutTracer
```

This style is intended to discourage the use of very big types like text or JSON
within programs which do logging. When expressed in this style, such a program
will not even have the possibility of doing the printf debugging style referred
to in the opening paragraph, because there is no `Tracer m Text` in scope!

```Haskell
-- Some action that can use any tracer on Event in IO.
-- It does not need to be able to log text in order to run.
action :: Tracer IO Event -> IO Int
action tracer = do
  traceWith tracer EventA
  stuff <- doSomething
  traceWith tracer (EventB 42)
  pure stuff

main :: IO ()
main = do
  _ <- action eventTracer
  pure ()
```

## Arrow tracers

The contravariant tracer is defined in terms of the arrow tracer found in the
module `Control.Tracer.Arrow`. The motivation for the arrow tracer is to
encourage the programmer to throw in `traceWith` calls liberally, and leave
them in so that they will not bit-rot, with the understanding that there will
be no runtime overhead if tracing is disabled. This module is only relevant when
dealing with a tracer which will ignore certain inputs but not others. With the
arrow representation, it is often possible to judge that a tracer will not emit
anything, without forcing the input, so that the `traceWith` call becomes a
no-op.
