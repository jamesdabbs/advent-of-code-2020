_An advent driver_

Assuming your project has the following file structure

    .
    └─── src
        ├── P01.hs
        ├── P02.hs
        └── ...

where each `P\d{2}.hs` module defines a `solution :: Solution`,
you can autogenerate a `Problems` set with e.g.

```haskell
# In src/Problems.hs
{-# OPTIONS_GHC -F -pgmF santa-discover #-}
```

and run it with

```haskell
# In app/Main.hs
import Problems (problems)
import Santa (defaultMain)

main :: IO ()
main = defaultMain problems "/path/to/inputs"
```

See `src/Santa/Main.hs` or run with `--help` for a full list of supported options.
