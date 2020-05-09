{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------

module AATree (
  AATree,        -- type of AA search trees
  emptyTree,     -- AATree a
  get,           -- Ord a => a -> AATree a -> Maybe a
  insert,        -- Ord a => a -> AATree a -> AATree a
  inorder,       -- AATree a -> [a]
  size,          -- AATree a -> Int
  height,        -- AATree a -> Int
  checkTree      -- Ord a => AATree a -> Bools
 ) where


-- import Data.Empty
--------------------------------------------------------------------------------
 -- Leaf {
                --   level::Int,
                --   value::a }
-- AA search trees

data AATree a = Empty   
              | Node { 
                  level :: Int,
                  left :: AATree a,
                  value :: a,
                  right :: AATree a }
                    deriving (Eq, Show, Read)

-- type Leaf a = Node level Empty a Empty 

emptyTree :: AATree a
emptyTree = Empty



get :: Ord a => a -> AATree a -> Maybe a
get _ Empty = Nothing
get a (Node _ left value right) 
  | a>value =get a right   
  | a<value =get a left   
  | a == value = Just a   


-- You may find it helpful to define

-- and call these from insert.
insert :: Ord a => a -> AATree a -> AATree a
insert = error "insert not implemented"

split :: AATree a -> AATree a
split = error "insert not implemented"

skew  :: AATree a -> AATree a
skew  =error "insert not implemented"

inorder :: AATree a -> [a]
inorder = error "inorder not implemented"

size :: AATree a -> Int
size (Empty) = 0
size (Node _ a _ b) = 1 + size a + size b


height :: AATree a -> Int
height Empty = 0
height (Node _ _ _ r) = 1 + height r  



-- OBS inte från labben ville testa hur bst insert fungerar
bstInsert:: Ord a => a-> AATree a -> AATree a
bstInsert a Empty = Node 1 Empty a Empty 
bstInsert  a (Node level left value right) 
          | a==value    =Node level left value right   
          | a>value    =Node level left  value (bstInsert a right)    
          | a<value    =Node level (bstInsert a left) value right    


--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants

checkTree :: Ord a => AATree a -> Bool
checkTree root =
  isSorted (inorder root) &&
  all checkLevels (nodes root)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x:nodes (leftSub x) ++ nodes (rightSub x)

-- True if the given list is ordered
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x1:x2:xs) = x1 < x2 && isSorted(x2:xs)


-- Check if the invariant is true for a single AA node
-- You may want to write this as a conjunction e.g.
  -- checkLevels node =
  --   leftChildOK node &&
  --   rightChildOK node &&
  --   rightGrandchildOK node
-- where each conjunct checks one aspect of the invariant

checkLevels :: AATree a -> Bool -- which checks that a single node satisfies its part of the invariant,
checkLevels node = leftChildOK node &&
                  rightChildOK node &&
                  rightGrandchildOK node
checkLevels Empty = True



leftChildOK :: AATree a -> Bool
leftChildOK (Node _ (Node _ Empty _ Empty) _ _) = True
leftChildOK (Node _ (Node _ Empty _ _) _ _ ) = False
leftChildOK (Node _ (Node _ _ _ Empty) _ _ ) = False
leftChildOK _ = True

rightChildOK  :: AATree a -> Bool 
rightChildOK (Node _ _ _ (Node _ Empty _ Empty)) = True
rightChildOK (Node _ _ _ (Node _ Empty _ _)) = False
rightChildOK (Node _ _ _ (Node _ _ _ Empty)) = False
rightChildOK _ = True

rightGrandchildOK :: AATree a -> Bool
rightGrandchildOK (Node _ _ _ a) = rightChildOK a && leftChildOK a

isEmpty :: AATree a -> Bool
isEmpty Empty = True
isEmpty _ = False

leftSub :: AATree a -> AATree a
leftSub (Node _ b _ _) = b
leftSub Empty = Empty


rightSub :: AATree a -> AATree a
rightSub (Node _ _ _ d) = d
rightSub Empty = Empty

--------------------------------------------------------------------------------

