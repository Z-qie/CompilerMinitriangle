{-
State Monad by Ziqi Yang, 2022

This file defines a State and StateIO monad used for the compiler.
-}

module State where

import Control.Applicative
import Data.Char
import System.IO

newtype ST st a = S (st -> (a, st))
newtype StateIO st res = StT (st -> IO (res, st))

app :: ST st a -> st -> (a, st)
app (S s) = s

appIO :: StateIO st a -> st -> IO (a, st)
appIO (StT trans) = trans

stUpdate :: st -> ST st ()
stUpdate s = S (\_ -> ((), s))

stGet :: ST st st
stGet = S (\s -> (s, s))

stGetIO :: StateIO st st
stGetIO = StT(\st -> do return (st, st))

stRevise :: (st -> st) -> ST st ()
stRevise f = stGet >>= stUpdate . f

stReviseIO :: (st -> st) -> StateIO st ()
stReviseIO f = do st <- stGetIO
                  stUpdateIO (f st)

lift :: IO a -> StateIO st a
lift m = StT(\s -> do x <- m
                      return (x, s))

stUpdateIO :: st -> StateIO st ()
stUpdateIO s = StT(\_ -> do return ((), s))

instance Functor (ST st) where
  -- fmap :: (a -> b) -> ST st a -> ST st b
  fmap g sta = S( \s -> let (x, s1) = app sta s
                        in (g x, s1))

instance Applicative (ST st) where
  pure x = S (\s -> (x, s))
  -- (<*>) :: (ST st (a -> b)) -> (ST st a)
  --                           -> (ST st b)
  stf <*> sta =
    S
      ( \s ->
          let (f, s1) = app stf s
              (x, s2) = app sta s1
           in (f x, s2)
      )

instance Monad (ST st) where
  return = pure

  -- (>>=) :: (ST st a) -> (a -> ST st b) -> ST st b
  sta >>= f =
    S
      ( \s ->
          let (x, s1) = app sta s
              (y, s2) = app (f x) s1
           in (y, s2)
      )

instance Functor (StateIO st) where
  -- fmap  :: (a->b) -> StateIO st a -> StateIO st b
  fmap g sta =
    StT
      ( \s -> do
          (x, s') <- appIO sta s
          return (g x, s')
      )

instance Applicative (StateIO st) where
  --pure :: a -> ST st a
  pure x = StT (\st -> return (x, st))

  --(<*>) :: ST st (sta -> stb) -> ST st sta -> ST st stb
  stf <*> sta =
    StT
      ( \st -> do
          (f, st1) <- appIO stf st
          (x, st2) <- appIO sta st1
          return (f x, st2)
      )

instance Monad (StateIO st) where
  -- return :: a -> StateIO st a
  -- return = pure

  -- ()>>=) :: StateIO st a -> (a -> StateIO st b) -> StateIO st b
  sta >>= fstb =
    StT
      ( \st -> do
          (x, st1) <- appIO sta st
          appIO (fstb x) st1
      )
