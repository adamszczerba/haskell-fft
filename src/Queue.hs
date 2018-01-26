module Queue
  ( Queue
  , newQueue
  , push
  , pop
  , peek
  , toList
  , size
  ) where


data Queue a = Queue {
  inbox  :: [a],
  outbox :: [a],
  size   :: Int
} deriving (Show);


-- | Normalize a queue, i.e. assert that the outbox is not empty
-- if the queue is not empty.
normalize :: Queue a -> Queue a
normalize (Queue inb [] size) = Queue [] (reverse inb) size
normalize x                   = x


-- | Queue to list.
toList :: Queue a -> [a]
toList (Queue inb outb _) = outb ++ (reverse inb)


-- | Create a new queue.
newQueue :: Queue a
newQueue = Queue { inbox = [], outbox = [], size = 0 }


-- | Add an element to the queue.
push :: a -> Queue a -> Queue a
push el (Queue inb outb size) = Queue (el:inb) outb (size + 1);


-- | Get the first added element and remove it.
pop :: Queue a -> (a, Queue a)
pop q = (o, Queue inb outb (size - 1))
  where (Queue inb (o:outb) size) = normalize q


-- | Get first added element.
peek :: Queue a -> a
peek q = (head outb)
  where (Queue _ outb _) = normalize q


instance Functor Queue where
  fmap f q = Queue [] (fmap f (toList q)) size
    where Queue _ _ size = q


instance Foldable Queue where
  foldr f z q = foldr f z (toList q)


instance Eq a => Eq (Queue a) where
  q1 == q2 = outb1 == outb2
    where (Queue _ outb1 _) = normalize q1
          (Queue _ outb2 _) = normalize q2
