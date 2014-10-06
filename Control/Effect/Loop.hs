{-# LANGUAGE DataKinds, DeriveDataTypeable, DeriveFunctor, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, LambdaCase, LiberalTypeSynonyms             #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction, RankNTypes   #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators               #-}
{-# LANGUAGE UndecidableInstances                                           #-}
module Control.Effect.Loop (Loop, EffectLoop, loop, stepLoop,
                            LoopState, loop', toCPS, fromCPS,
                            continue, exit, continueWith, exitWith,
                            foreach, while, doWhile, once,
                            repeatLoop, iterateLoop) where
import Control.Effect
import Control.Monad  (when)
import Data.Data      (Typeable)

-- | @Loop c e@ indicates an effect with ability to 'continue' with @c@ or 'exit' with @e@.
newtype Loop c e a
  = Loop (forall r. (c -> r) -> (e -> r) -> (a -> r) -> r)
  deriving (Typeable, Functor)

type instance Is Loop f = IsLoop f

type family IsLoop f where
  IsLoop (Loop c e) = True
  IsLoop f          = False

-- | Lift a CPS style computation with continuation and exit handler to the 'Effect' monad.
loop :: EffectLoop c e l => (forall r. (c -> r) -> (e -> r) -> (a -> r) -> r) -> Effect l a
loop f = send $ Loop f
{-# INLINE loop #-}

fromCPS :: Loop c e a -> LoopState c e a
fromCPS (Loop f) = f ContinueWith ExitWith Return

toCPS :: LoopState c e a -> Loop c e a
toCPS st = Loop $ \c2r e2r a2r ->
  case st of
    ContinueWith c -> c2r c
    ExitWith     e -> e2r e
    Return       a -> a2r a

data LoopState c e a = ContinueWith c
                     | ExitWith e
                     | Return a
                       deriving (Read, Show, Eq, Ord)

loop' :: EffectLoop c e l => LoopState c e a -> Effect l a
loop' = send . toCPS
{-# INLINE loop' #-}

stepLoop :: Effect (Loop c e :+ l) c -> (c -> Effect l e) -> Effect l e
stepLoop act cont = eliminate cont handle act
  where
    handle (Loop f) = f cont return id

class MemberEffect Loop (Loop c e) l => EffectLoop c e l
instance MemberEffect Loop (Loop c e) l => EffectLoop c e l

continueWith :: forall c e l a. EffectLoop c e l => c -> Effect l a
continueWith = loop' . ContinueWith
{-# INLINE continueWith #-}

continue :: EffectLoop () e l => Effect l a
continue = continueWith ()
{-# INLINE continue #-}

exitWith :: EffectLoop c e l => e -> Effect l a
exitWith = loop' . ExitWith
{-# INLINE exitWith #-}

exit :: EffectLoop c () l => Effect l a
exit = exitWith ()
{-# INLINE exit #-}

foreach :: [a] -> (a -> Effect (Loop c () :+ l) c) -> Effect l ()
foreach xs body = looper xs
  where
    looper []        = return ()
    looper (x : xs') = stepLoop (body x) $ \_ -> looper xs'
{-# INLINE foreach #-}

while :: Effect l Bool -> Effect (Loop c () :+ l) c -> Effect l ()
while cond body = looper
  where
    looper = do
      p <- cond
      when p $ stepLoop body $ \_ -> looper
{-# INLINE while #-}

doWhile :: Effect (Loop a a :+ l) a -> Effect l Bool -> Effect l a
doWhile body cond = looper
  where
    looper = stepLoop body $ \a -> do
      p <- cond
      if p then looper else return a
{-# INLINE doWhile #-}

once :: Effect (Loop a a :+ l) a -> Effect l a
once body = eliminate return handler body
  where
    handler (Loop f) = f return return id
{-# INLINE once #-}

repeatLoop :: Effect (Loop c e :+ l) a -> Effect l e
repeatLoop body = looper
  where
    looper = eliminate (const looper) handler body
    handler (Loop f) = f (const looper) return id
{-# INLINE repeatLoop #-}

iterateLoop :: c -> (c -> Effect (Loop c e :+ l) c) -> Effect l e
iterateLoop z body = looper z
  where
    looper c = stepLoop (body c) looper
{-# INLINE iterateLoop #-}
