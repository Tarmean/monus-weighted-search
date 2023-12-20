{-# Language UndecidableInstances #-}
module Control.Monad.CpsHeap where
import Control.Monad ( MonadPlus(..), liftM, liftM2 )
import Control.Monad.Trans ( MonadTrans(..) )
import Control.Monad.State ( MonadState(put, get), StateT (runStateT), modify )
import Control.Applicative
    ( Applicative(liftA2), Alternative(empty, (<|>)) )
import Data.Functor.Identity ( Identity(runIdentity) )
import Data.Foldable (asum)
import GHC.Exts (oneShot)
import GHC.OldList (sortOn)
import Data.List (groupBy)
import Data.Function (on)


type CPS s r a = (a -> s -> (s -> r) -> r) -> s -> (s -> r) -> r

newtype HeapT w m a = HeapT { unHeap :: forall r. CPS (Slice w (m r)) (m r) a }
type Heap w = HeapT w Identity

data Slice w r
    = NoSlice
    | SliceOne !w (SliceCont w r)
    | SliceTwo !w (SliceCont w r) !w (SliceCont w r)

type SliceCont w r = Slice w r -> (Slice w r -> r) -> r

{-# INLINE altSliceCont #-}
altSliceCont :: SliceCont w r -> SliceCont w r -> SliceCont w r
altSliceCont f g = oneShot $ \sl cont -> f sl (oneShot $ \sl' -> g sl' cont)

{-# INLINE fromList #-}
fromList :: (MonadWeight w m, Monoid w) => [(a,w)] -> m a
fromList ls = go $ map norm $ groupBy ((==) `on` snd) . sortOn snd $ ls
  where
    norm ((a,w):ls) = (w, a:fmap fst ls)
    norm [] = (mempty, [])

    go ((w, xs):ys) = do
        pauseWith (\_ -> w)
        asum (fmap pure xs) <|> go ys
    go [] = empty

instance Functor (HeapT w m) where
  {-# INLINE fmap #-}
  fmap = liftM
instance Applicative (HeapT w m) where
  {-# INLINE pure #-}
  pure a = HeapT $ oneShot $ \onSucc onStep onFail -> onSucc a onStep onFail
  {-# INLINE liftA2 #-}
  liftA2 = liftM2
instance Monad (HeapT w m) where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  HeapT k >>= f = HeapT $ oneShot $ \onSucc onStep onFail -> k (oneShot $ \a onStep' onFail' -> unHeap (f a) onSucc onStep' onFail') onStep onFail
instance Alternative (HeapT w m) where
   {-# INLINE empty #-}
   empty = HeapT $ oneShot $ \_ s onFail -> onFail s
   {-# INLINE (<|>) #-}
   l <|> r = HeapT $ oneShot $ \onSucc onStep onFail -> unHeap l onSucc onStep (oneShot $ \slice' -> unHeap r onSucc slice' onFail)
instance MonadPlus (HeapT w m) where
   {-# INLINE mzero #-}
   mzero = empty
   {-# INLINE mplus #-}
   mplus = (<|>)
instance MonadTrans (HeapT w) where
  {-# INLINE lift #-}
  lift m = HeapT $ oneShot $ \onSucc slice onFail -> m >>= oneShot (\a -> onSucc a slice onFail)
instance MonadState s m => MonadState s (HeapT w m) where
  {-# INLINE get #-}
  {-# INLINE put #-}
  get = lift get
  put = lift . put

pause :: Ord w => w -> HeapT w m ()
pause w = HeapT $ oneShot $ \onSucc stack onFail -> onFail (pushSlice w (onSucc ()) stack)

-- instance Monus w => MonadWriter w (HeapT w m) where
--   {-# INLINE tell #-}
--   tell w = HeapT $ oneShot $ \onSucc stack onFail -> onFail (pushSlice w (onSucc ()) stack)
--   listen _ = error "not implemented"
--   pass m = do
--     (a,w) <- runWriterT . pass . lift $ m
--     tell w
--     pure a

{-# INLINE pushSlice #-}
pushSlice :: (Ord w) => w -> SliceCont w r -> Slice w r -> Slice w r
pushSlice w f NoSlice = SliceOne w f
pushSlice w f (SliceOne w0 v0) = SliceTwo w0 v0 w f
pushSlice w f (SliceTwo w0 v0 w1 v1) = case mergeSlice w0 v0 (mergeSlice w1 v1 (w, f)) of
  (w', f') -> SliceOne w' f'

{-# INLINE mergeSlice #-}
mergeSlice :: (Ord w) => w -> SliceCont w r ->(w, SliceCont w r) -> (w, SliceCont w r)
mergeSlice wl l (wr, r) = case compare wl wr of
   EQ -> (wl, altSliceCont l r)
   LT -> (wl, oneShot $ \x -> l (pushSliceRec wr r x))
   GT -> (wr, oneShot $ \x -> r (pushSliceRec wl l x))


pushSliceRec :: (Ord w) => w -> SliceCont w r -> Slice w r -> Slice w r
pushSliceRec = pushSlice

-- searchT :: (Ord w, Applicative m) => HeapT w m a -> m [(a, w)]
-- searchT = runHeapT . trackWeight

-- search :: (Ord w) => HeapT w (TrackWeightT w Identity) a -> [(a,w)]
-- search h = build $ oneShot $ \cons zero -> execHeapT h (oneShot $ \a m -> (a`cons`) <$> m) (oneShot $ \w m -> tell w *> m) (pure zero)





{-# INLINE search #-}
search :: Ord w => w -> TrackWeightT w Identity a -> [(a, w)]
search w0 = runIdentity . searchT w0
{-# INLINE searchT #-}
searchT :: (Applicative m, Ord w1) => w1 -> TrackWeightT w1 m a -> m [(a, w1)]
searchT w0 = runHeapT . runTrackWeight w0
  where 

    {-# INLINE runHeapT #-}
    runHeapT h =  execHeapT h (oneShot $ \a m -> (a:) <$> m) (oneShot $ \_ m -> m) (pure [])


{-# INLINE execHeapT #-}
execHeapT :: ( Ord w) => HeapT w m a -> (a -> m r -> m r) -> (w -> m r -> m r) -> m r -> m r
execHeapT h onSucc onWeight onFail = case h of
   HeapT k -> k (\a s f -> onSucc a (f s)) NoSlice (oneShot $ \slice -> runSlice onWeight onFail slice)

{-# INLINE runSlice #-}
runSlice :: Ord w => (w -> r -> r) -> r -> Slice w r -> r
runSlice onWeight onFail = go
  where
    go NoSlice = onFail
    go (SliceOne w a) = onWeight w $ a NoSlice go
    go (SliceTwo w a w2 b) = case compare w w2 of
         LT -> onWeight w $ a (SliceOne w2 b) go
         GT -> onWeight w2 $ b (SliceOne w a) go
         EQ -> onWeight w $ a NoSlice (`b` go)

newtype TrackWeightT w m a = TrackWeightT { getTrackWeightT :: StateT w (HeapT w m) a}
  deriving (Functor, Monad, Applicative, Alternative, MonadPlus)
instance MonadTrans (TrackWeightT w) where
  lift = TrackWeightT . lift . lift
class (Alternative m, Ord w, Monad m) => MonadWeight w m | m -> w where
   getWeight ::m w
   pauseWith :: (w -> w) -> m ()

instance MonadState s m => MonadState s (TrackWeightT w m) where
    get = lift get
    put = lift . put
instance (w ~ w1, Ord w) => MonadWeight w1 (TrackWeightT w m) where
    {-# INLINE getWeight #-}
    {-# INLINE pauseWith #-}
    getWeight = TrackWeightT get
    pauseWith f = TrackWeightT $ do
        modify f
        w <- get
        lift (pause w)
-- instance (MonadWriter w m, Semigroup w) => MonadWriter w (TrackWeightT w m) where
--     {-# INLINE tell #-}
--     {-# INLINE listen #-}
--     tell w = TrackWeightT $ modify (<> w) *> lift (tell w)
--     listen (TrackWeightT twt) = TrackWeightT (listen twt)
--     pass (TrackWeightT rt) = TrackWeightT (pass rt)
-- {-# INLINE runTrackWeight #-}
runTrackWeight :: w -> TrackWeightT w m a -> HeapT w m (a, w)
runTrackWeight w0 t = flip runStateT w0 $ getTrackWeightT t
