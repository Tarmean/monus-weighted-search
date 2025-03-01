{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.Heap
-- Copyright   : (c) Donnacha Oisín Kidney 2021
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : non-portable
--
-- The Heap monad: a monad for efficient weighted search.
--
-- This module provides an implementation of the Heap monad transformer as
-- described in:
--
-- * Donnacha Oisín Kidney and Nicolas Wu. 2021. /Algebras for weighted search/.
--   Proc. ACM Program. Lang. 5, ICFP, Article 72 (August 2021), 30 pages.
--   DOI:<https://doi.org/10.1145/3473577>
--
-- This monad transformer can be used to implement search algorithms like
-- Dijkstra's algorithm (see "MonusWeightedSearch.Examples.Dijkstra"), or the
-- Viterbi algorithm ("MonusWeightedSearch.Examples.Viterbi"), or probabilistic
-- parsing ("MonusWeightedSearch.Examples.Parsing").
--
-- The type supports nondeterminism (using the 'Alternative' and 'MonadPlus'
-- interfaces), where each branch in a computation can be weighted by some
-- 'Monus'. A 'Monus' is an ordered 'Monoid' with some pseudo-subtraction
-- operator, see the "Data.Monus" module for more details.
--------------------------------------------------------------------------------


module Control.Monad.Heap
  ( -- * Heap Type
    HeapT(..)
  , Node(..)
    -- ** Non-transformer form
  , Heap
  , pattern Heap
  , runHeap

    -- * Constructing Heaps
  , fromList

    -- * Popping the smallest elements
  , popMin
  , popMinT
  
    -- * Popping the smallest element
  , popMinOne
  , popMinOneT

    -- * Turning into a cons-list
  , flatten
  , flattenT

    -- * Searching the whole heap
  , search
  , searchT

    -- * Returning one element
  , best
  , bestT
  )
  where

import Data.Bifunctor ( Bifunctor(..) )
import Data.Bifoldable ( Bifoldable(..), bifoldl', bifoldr' )
import Data.Bitraversable ( Bitraversable(..) )
import Control.Monad.Heap.List
    ( toListT, ListCons(..), ListT(..) )
import Control.Monad ( MonadPlus , (<=<))
import Control.Applicative
    ( Applicative(liftA2), Alternative(empty, (<|>)) )
import Control.Monad.Trans ( MonadTrans(..) )
import Data.Monus ( Monus(..) )
import Control.Monad.Writer ( MonadWriter(..), WriterT(..) )
import Control.Monad.State ( MonadState(..) )
import Control.Monad.Except ( MonadError(..) )
import Control.Monad.Reader ( MonadReader(..) )
import Control.Monad.Cont ( MonadCont(..) )
import Data.Functor.Identity ( Identity(..) )
import Test.QuickCheck
    ( arbitrary1,
      frequency,
      sized,
      Arbitrary(arbitrary),
      Arbitrary1(liftArbitrary) )
import MonusWeightedSearch.Internal.CoerceOperators
    ( (<#$>), (.#), (#.), under )
import MonusWeightedSearch.Internal.TestHelpers ( sumsTo )
import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic, Generic1 )
import Control.DeepSeq ( NFData(..) )
import Data.Foldable (Foldable(foldl', foldr'))
import Text.Read (readPrec, parens, prec, Lexeme(Ident), lexP, step)
import Data.Monoid (Alt(Alt))
import Control.Monad.Free.Class
import GHC.Exts (IsList)
import qualified GHC.Exts as IsList

-- $setup
-- >>> import Data.Monus.Dist
-- >>> default (Dist,Integer,Double)

infixr 5 :<
-- | A 'Heap' is a list of 'Node's of 'Heap's.
data Node w a b = Leaf a | !w :< b
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Data, Typeable, Generic, Generic1)

instance (NFData w, NFData a, NFData b) => NFData (Node w a b) where
  rnf (Leaf x) = rnf x
  rnf (x :< xs) = rnf x `seq` rnf xs
  {-# INLINE rnf #-}

instance Bifunctor (Node w) where
  bimap f g (Leaf x) = Leaf (f x)
  bimap f g (x :< xs) = x :< g xs

  first f (Leaf x) = Leaf (f x)
  first f (x :< xs) = x :< xs

  second f (Leaf x) = Leaf x
  second f (x :< xs) = x :< f xs

  {-# INLINE bimap #-}
  {-# INLINE first #-}
  {-# INLINE second #-}

instance Bifoldable (Node w) where
  bifold (Leaf x) = x
  bifold (_ :< x) = x
  {-# INLINE bifold #-}

  bifoldMap f _ (Leaf x) = f x
  bifoldMap _ f (_ :< x) = f x
  {-# INLINE bifoldMap #-}

  bifoldr f _ b (Leaf x) = f x b
  bifoldr _ f b (_ :< x) = f x b
  {-# INLINE bifoldr #-}

  bifoldl f _ b (Leaf x) = f b x
  bifoldl _ f b (_ :< x) = f b x
  {-# INLINE bifoldl #-}

instance Bitraversable (Node w) where
  bitraverse f _ (Leaf x) = fmap Leaf (f x)
  bitraverse _ f (x :< xs) = fmap (x :<) (f xs)
  {-# INLINE bitraverse #-}

-- | The 'HeapT' monad transformer: a monad for weighted search.
--
-- This monad supports nondeterminism through the 'Alternative' and
-- 'MonadPlus' classes, but different branches in the computation may be
-- weighted by the @w@ parameter. A computation can be given a specific weight
-- using the 'MonadWriter' interface:
--
-- @
--   'tell' 4 '>>' xs
-- @
--
-- This weights the computation @xs@ with @4@.
--
-- Depending on the 'Monus' used, the order of the search can be specified.
-- For instance, using the 'Monus' in "Data.Monus.Dist", we have the following:
--
-- >>> search (fromList [('a',5), ('b', 3), ('c',6)])
-- [('b',3),('a',5),('c',6)]
--
-- >>> search (fromList [('b',3), ('a',5), ('c',6)])
-- [('b',3),('a',5),('c',6)]
newtype HeapT w m a = HeapT { runHeapT :: ListT m (Node w a (HeapT w m a)) }
  deriving (Typeable, Generic)
  deriving (Semigroup, Monoid) via Alt (HeapT w m) a

deriving instance (forall x. Data x => Data (m x), Typeable m, Data a, Data w) => Data (HeapT w m a)

-- | Build a heap from a list of values paired with their weights.
fromList :: Applicative m => [(a,w)] -> HeapT w m a
fromList = HeapT #. foldr f (ListT (pure Nil))
  where
    f (x,w) xs = ListT (pure ((w :< HeapT (ListT (pure (Leaf x :- ListT (pure Nil))))) :- xs))
{-# INLINE fromList #-}

instance Monad m => MonadFree ((,) w) (HeapT w m) where
  wrap (w, xs) = HeapT (pure (w :< xs))
  {-# INLINE wrap #-}

instance Foldable m => Foldable (HeapT w m) where
  foldr f = go
    where
      go = (. runHeapT) #. foldr (flip (bifoldr f (flip go)))
  {-# INLINE foldr #-}
  
  foldMap f = go
    where
      go = foldMap (bifoldMap f go) .# runHeapT
  {-# INLINE foldMap #-}

  foldl f = go
    where
      go = (. runHeapT) #. foldl (bifoldl f go)
  {-# INLINE foldl #-}
  
  foldl' f = go
    where
      go = (. runHeapT) #. foldl' (bifoldl' f go)
  {-# INLINE foldl' #-}
  
  foldr' f = go
    where
      go = (. runHeapT) #. foldr' (flip (bifoldr' f (flip go)))
  {-# INLINE foldr' #-}

instance Traversable m => Traversable (HeapT w m) where
  traverse :: forall f a b. Applicative f => (a -> f b) -> HeapT w m a -> f (HeapT w m b)
  traverse f = fmap (HeapT #. ListT) . (traverse h .# (runListT . runHeapT))
    where
      h :: ListCons (Node w a (HeapT w m a)) (ListT m (Node w a (HeapT w m a))) -> 
           f (ListCons (Node w b (HeapT w m b)) (ListT m (Node w b (HeapT w m b))))
      h Nil = pure Nil
      h (Leaf x :- ListT xs) = liftA2 ((. ListT) #. (:-) . Leaf) (f x) (traverse h xs)
      h ((x :< HeapT (ListT xs)) :- ListT xss) = liftA2 (((. ListT) #. (:-)) . ((x :<) .# (HeapT . ListT))) (traverse h xs) (traverse h xss)
  {-# INLINE traverse #-}

deriving newtype instance (forall x. NFData x => NFData (m x), NFData w, NFData a) => NFData (HeapT w m a) 

instance (Arbitrary1 m, Arbitrary w, Arbitrary a) => Arbitrary (HeapT w m a) where
  arbitrary = arbitrary1

instance (Arbitrary1 m, Arbitrary w) => Arbitrary1 (HeapT w m) where
  liftArbitrary arb = sized go1
    where
      go1 n = HeapT <#$> (sumsTo n >>= foldr go2f go2b)
      go2b      = ListT <#$> liftArbitrary (pure Nil)
      go2f n ns = ListT <#$> liftArbitrary (liftA2 (:-) (go3 n) ns)
      go3 n | n <= 1 = fmap Leaf arb
      go3 n = frequency [(1, fmap Leaf arb), (n, liftA2 (:<) arbitrary (go1 n))]

-- | The 'Heap' type, specialised to the 'Identity' monad.
type Heap w = HeapT w Identity

runHeapIdent :: Heap w a -> [Node w a (Heap w a)]
runHeapIdent = runIdentity #. (toListT .# runHeapT)
{-# INLINE runHeapIdent #-}

toHeapIdent :: [Node w a (Heap w a)] -> Heap w a
toHeapIdent = HeapT #. foldr (((ListT . Identity) . ) #. (:-)) (ListT (Identity Nil))
{-# INLINE toHeapIdent #-}

-- | The constructor for the non-transformer 'Heap' type.
pattern Heap :: [Node w a (Heap w a)] -> Heap w a
pattern Heap { runHeap } <- (runHeapIdent -> runHeap) 
  where
    Heap = toHeapIdent
{-# COMPLETE Heap #-}

instance (forall x. Show x => Show (m x), Show a, Show w) => Show (HeapT w m a) where
  showsPrec n (HeapT xs) = showParen (n > 10) (showString "HeapT " . showsPrec 11 xs)
  
instance (forall x. Read x => Read (m x), Read w, Read a) => Read (HeapT w m a) where
  readPrec = parens $
      prec 10 $ do
        Ident "HeapT" <- lexP
        m <- step readPrec
        return (HeapT m)
  
deriving instance (forall x. Eq x => Eq (m x), Eq a, Eq w) => Eq (HeapT w m a)
-- Some special incantations are needed to make this work:
-- In my mind, the following *should* work:
-- @
-- deriving instance (Ord w, Ord a, forall x. Ord x => Ord (m x)) => Ord (HeapT w m a)
-- @
-- But for reasons described here
-- https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/9.0.1-notes.html#language
-- You need the following slightly more complicated thing:
deriving instance ( Ord w, Ord a
                  , forall x. Ord x => Ord (m x)
                  , Eq (HeapT w m a)                       -- These two are needed
                  , Eq (ListT m (Node w a (HeapT w m a)))  -- for reasons I do not understand!
                  ) => Ord (HeapT w m a)

instance Functor m => Functor (HeapT w m) where
  fmap f = HeapT #. (fmap (bimap f (fmap f)) .# runHeapT)
  {-# INLINE fmap #-}

instance Monad m => Applicative (HeapT w m) where
  pure = HeapT #. pure . Leaf
  {-# INLINE pure #-}
  (<*>) = liftA2 id
  {-# INLINE (<*>) #-}
  (*>) = (>>)  -- We have to do this because the default definition
               -- is (x *> y) = (id <$ x) <*> y. (which is horrifically slow)
  {-# INLINE (*>) #-}
  liftA2 f xs ys = do
    x <- xs
    fmap (f x) ys
  {-# INLINE liftA2 #-}

instance Monad m => Monad (HeapT w m) where
  HeapT m >>= f = HeapT (m >>= g)
    where
      g (Leaf x) = runHeapT (f x)
      g (w :< xs) = pure (w :< (xs >>= f))
  -- {-# INLINE (>>=) #-}
  xs >> ys = xs >>= const ys
  {-# INLINE (>>) #-}

instance Monad m => Alternative (HeapT w m) where
  (<|>) = under (<|>) runHeapT
  {-# INLINE (<|>) #-}
  empty = HeapT empty
  {-# INLINE empty #-}

instance Monad m => MonadPlus (HeapT w m)

instance MonadTrans (HeapT w) where
  lift = HeapT #. ListT #. fmap ((:- empty) . Leaf)
  {-# INLINE lift #-}

(<||>) ::  (Monus w, Monad m) =>
           (w, HeapT w m a) ->
           (w, HeapT w m a) ->
           (w, HeapT w m a)
(x, xv) <||> (y, yv)
  | x <= y    = (x, HeapT (ListT (pure ((y |-| x :< yv) :- runHeapT xv))))
  | otherwise = (y, HeapT (ListT (pure ((x |-| y :< xv) :- runHeapT yv))))
{-# INLINE (<||>) #-}

comb ::  (Monus w, Monad m) =>
         [(w, HeapT w m a)] ->
         Maybe (w, HeapT w m a)
comb [] = Nothing
comb (x:xs) = Just (comb1 x xs)
  where
    comb1 x [] = x
    comb1 x1 [x2] = x1 <||> x2
    comb1 x1 (x2 : x3 : xs) = (x1 <||> x2) <||> comb1 x3 xs
{-# INLINE comb #-}

partition :: [Node w a b] -> ([a], [(w, b)])
partition = foldr f ([],[])
  where
    f (Leaf x) (ys,zs) = (x:ys,zs)
    f (w :< x) (ys,zs) = (ys, (w, x) :zs)
{-# INLINE partition #-}

-- | The monadic variant of 'popMin'.
popMinT ::  (Monus w, Monad m) =>
            HeapT w m a ->
            m ([a], Maybe (w, HeapT w m a))
popMinT = fmap (second comb . partition) . toListT .# runHeapT
{-# INLINE popMinT #-}

-- | /O(log n)/. 'popMin' returns a list of those elements in the 'Heap' with a
-- weight equal to 'mempty', paired with the rest of the heap and the minimum
-- weight in the rest of the heap.
popMin :: Monus w => Heap w a -> ([a], Maybe (w, Heap w a))
popMin = runIdentity #. popMinT
{-# INLINE popMin #-}

-- | The monadic variant of 'popMinOne'.
popMinOneT :: forall w m a. (Monus w, Monad m) => HeapT w m a -> m (Maybe ((a, w), HeapT w m a))
popMinOneT = go mempty [] .# runHeapT
  where
    go' :: w -> Maybe (w, HeapT w m a) -> m (Maybe ((a, w), HeapT w m a))
    go' a Nothing = pure Nothing
    go' a (Just (w, HeapT xs)) = go (a <> w) [] xs
    
    go :: w -> [(w, HeapT w m a)] -> ListT m (Node w a (HeapT w m a)) -> m (Maybe ((a, w), HeapT w m a))
    go w a (ListT xs) = xs >>= \case
      Nil -> go' w (comb (reverse a))
      Leaf x :- xs -> pure (Just ((x, w), tell w >> HeapT (foldl (\ys (yw,y) -> ListT (pure ((yw :< y) :- ys))) xs a)))
      (u :< x) :- xs -> go w ((u,x) : a) xs
{-# INLINABLE popMinOneT #-}
    
-- | /O(log n)/. 'popMinOne' returns the smallest weighted element in the
-- heap, along with its weight, along with the rest of the heap.
--
-- This function is a more traditional version of 'popMin', but it has much
-- worse performance in general.
popMinOne :: Monus w => Heap w a -> Maybe ((a, w), Heap w a)
popMinOne = runIdentity #. popMinOneT
{-# INLINABLE popMinOne #-}

-- | The monadic version of 'flatten'.
flattenT :: (Monad m, Monoid w) => HeapT w m a -> ListT m (a, w)
flattenT = runWriterT #. go
  where
    go = (h <=< lift) .# runHeapT
    h (Leaf x) = return x
    h (w :< xs) = writer (xs, w) >>= go
{-# INLINABLE flattenT #-}

-- | /O(n)/. Return all the elements of the heap, /not/ in order of their
-- weights, paired with their weights.
--
-- >>> flatten (fromList [('a',5), ('b', 3), ('c',6)])
-- [('a',5),('b',3),('c',6)]
flatten :: Monoid w => Heap w a -> [(a, w)]
flatten = runIdentity #. toListT . flattenT
{-# INLINE flatten #-}

-- | The monadic variant of 'search'.
searchT ::  (Monad m, Monus w) =>
            HeapT w m a -> m [(a, w)]
searchT xs = popMinT xs >>= go mempty where
    go !w1 (x, Nothing)        = pure  (map (,w1) x)
    go !w1 (x, Just (w2, xs))  = fmap  (map (,w1) x ++) (popMinT xs >>= go (w1 <> w2))
{-# INLINE searchT #-}

-- | /O(n log n)/. Return all of the elements in the heap, in order, paired
-- with their weights.
search :: Monus w => Heap w a -> [(a, w)]
search = runIdentity #. searchT
{-# INLINE search #-}

instance (Monoid w, m ~ Identity) => IsList (HeapT w m a) where
  type Item (HeapT w m a) = (a, w)
  fromList = fromList
  {-# INLINE fromList #-}
  toList = flatten
  {-# INLINE toList #-}

-- | The monadic variant of 'best'.
bestT :: (Monad m, Monus w) => HeapT w m a -> m (Maybe (w, a))
bestT = go mempty
  where
    go !a xs = popMinT xs >>= \case
      (y:_,_) -> pure (Just (a, y))
      ([],Nothing) -> pure Nothing
      ([],Just (w,zs)) -> go (a <> w) zs
{-# INLINE bestT #-}

-- | /O(log n)/. Return the lowest-weight element in the heap, paired with its
-- weight.
best :: Monus w => Heap w a -> Maybe (w, a)
best = runIdentity #. bestT
{-# INLINE best #-}

heapMmap :: forall m1 m2 a1 a2 w1 w2. Functor m1 =>
            (m1 (ListCons (Node w1 a1 (HeapT w2 m2 a2)) (ListT m2 (Node w2 a2 (HeapT w2 m2 a2)))) ->
             m2 (ListCons (Node w2 a2 (HeapT w2 m2 a2)) (ListT m2 (Node w2 a2 (HeapT w2 m2 a2)))))
         -> HeapT w1 m1 a1 -> HeapT w2 m2 a2
heapMmap h = HeapT #. (goL .# runHeapT)
  where
    goL :: ListT m1 (Node w1 a1 (HeapT w1 m1 a1)) -> ListT m2 (Node w2 a2 (HeapT w2 m2 a2))
    goL = ListT #. h . (fmap (bimap (fmap (HeapT #. (goL .# runHeapT))) goL) .# runListT)
{-# INLINE heapMmap #-}

instance (Monad m, Monoid w) => MonadWriter w (HeapT w m) where
  writer (x, !w) = HeapT (pure (w :< pure x))
  {-# INLINE writer #-}
  tell !w = HeapT (pure (w :< pure ()))
  {-# INLINE tell #-}
  listen = go mempty
    where
      go !w = HeapT #. (fmap (h w) .# runHeapT)
      h !w1 (Leaf x) = Leaf (x, w1)
      h !w1 (w2 :< xs) = w2 :< go (w1 <> w2) xs
  {-# INLINE listen #-}
  pass = HeapT #. fmap (\(x, w) -> w :< return x) . (runWriterT #. pass) . (WriterT #. flattenT)
  {-# INLINE pass #-}

instance MonadState s m => MonadState s (HeapT w m) where
  get = lift get
  put = lift . put
  state = lift . state
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE state #-}

instance MonadError e m => MonadError e (HeapT w m) where
  throwError = lift . throwError
  {-# INLINE throwError #-}
  catchError xs h = heapMmap (`catchError` ((runListT . runHeapT) #. h)) xs
  {-# INLINE catchError #-}

instance MonadReader r m => MonadReader r (HeapT w m) where
  ask = lift ask
  {-# INLINE ask #-}
  reader = lift . reader
  {-# INLINE reader #-}
  local = heapMmap . local
  {-# INLINE local #-}

instance MonadCont m => MonadCont (HeapT w m) where
  callCC f = HeapT (callCC (\c -> runHeapT (f (HeapT #. c . Leaf))))
  {-# INLINE callCC #-}
