{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: FilePath -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Tuple Functions that could help --

  fst :: (a, b) -> a
  snd :: (a, b) -> b
  (,) :: a -> b -> (a, b)

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Consideration --
  Try to avoid repetition. Factor out any common expressions.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ...
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- Given the file name, and file contents, print them.
-- Use @putStrLn@.
-- f a
-- a -> f ()
printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile fp c=
    putStrLn ("============ " ++ fp) >> putStrLn (c)

-- let ab = fp ++ "a"
--     a = ("=======" ++ ab)
--     b = putStrLn (a) >> putStrLn (c)
--  in b
-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles =
    void . sequence . (<$>) (uncurry printFile)

-- let a = (<$>) (uncurry printFile) :: List(FilePath, Chars) -> List( IO ())
--     b = void . sequence :: List(IO a) -> IO ()
--     c = b . a :: List(FilePath, Chars) -> IO ()
--     d = c xs
--  in d
-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile fp =
    (\c -> (fp, c)) <$> (readFile fp)
  

-- let a = readFile fp
--     b = (<$>) (\x -> (fp, x)) a
--  in b
-- let a = lift2 (<$>) (,) readFile
--  in a
-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles =
    sequence . (<$>) getFile
  
-- sequence (map getFile xs)
--
-- let a =  map getFile xs 
--     b = sequence a  
--  in b

-- Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()
run fp =
    readFile fp >>= getFiles . lines >>= printFiles
  

-- let a = readFile fp
--     b = (>>=) a (\x -> getFiles (lines (x))) 
--     c = (>>=) b (\xs -> printFiles xs)
--  in c
-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
    let a = getArgs
        b = a >>= (\xs -> 
            case xs of 
              Nil -> putStrLn("No elements")
              (x :. _) -> run x
            )
     in b

----

-- Was there was some repetition in our solution?
-- ? `sequence . (<$>)`
-- ? `void . sequence . (<$>)`
-- Factor it out.
