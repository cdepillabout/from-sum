## 0.2.3.0

*   Add functions `collapseErrExceptT`, `liftEitherExceptT`,
    `fromEitherExceptT`, `fromEitherOrExceptT`, `fromEitherMExceptT`,
    `fromEitherOrMExceptT`, `fromMaybeExceptT`, `fromMaybeOrExceptT`,
    `fromMaybeMExceptT`, and `fromMaybeOrMExceptT`.  These functions
    help convert `Maybe` and `Either` to `ExceptT`.
    [#2](https://github.com/cdepillabout/from-sum/pull/2)

## 0.2.2.0

*   Added new functions maybeToEither, maybeToEitherOr, and eitherToMaybe for
    converting between `Maybe` and `Either`.
    [#1](https://github.com/cdepillabout/from-sum/pull/1)
