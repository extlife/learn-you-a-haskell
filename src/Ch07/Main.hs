{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
import qualified Data.Map as Map

data Point = Point Float Float deriving(Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
    Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

data Person = Person { firstName :: String
                     , lastName :: String 
                     , age :: Int 
                     , height :: Float 
                     , phoneNumber :: String 
                     , flavor :: String
                     } deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a 
vplus  (Vector a b c) (Vector d e f) = Vector (a + d) (b + b) (c + f)

vectMult :: (Num a) => Vector a -> a -> Vector a
vectMult (Vector a b c) n = Vector (a * n) (b * n) (c * n) 

scalarMult :: (Num a) => Vector a -> Vector a -> a
scalarMult (Vector a b c) (Vector d e f) = a * d + b * e + c * f 

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook =      
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool 
inPhoneBook name number book = (name, number) `elem` book

type AssocList k v = [(k, v)]

data LockState = Taken | Free deriving (Show, Eq)
type Code = String 
type LockerMap = Map.Map Int (LockState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup number map = case Map.lookup number map of
    Nothing -> Left $ "Locker number " ++ show number ++ " doesn't exist!"  
    Just (state, code) -> if state /= Taken then Right code
        else Left $ "Locker " ++ show number ++ " is already taken!"  

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ] 

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x > a = Node a left (treeInsert x right) 
    | x < a = Node a (treeInsert x left) right
    | otherwise = Node x left right 

treeElem :: (Ord a) => a -> Tree a -> Bool 
treeElem _ EmptyTree = False 
treeElem x (Node a left right)
    | x > a = treeElem x right
    | x < a = treeElem x left
    | otherwise = True

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True 
    Yellow == Yellow = True 
    Green == Green = True 
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno Nothing = False 
    yesno _ = True

instance YesNo (Tree a) where
    yesno EmptyTree = False 
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False 
    yesno _ = True

yesnoIf :: (YesNo a) => a -> b -> b -> b
yesnoIf val yes no = if yesno val then yes else no

instance Functor Tree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

class Tofu t where  
    tofu :: j a -> t a j 

data Frank a b = Frank { frankField :: b a }

instance Tofu Frank where
    tofu = Frank

data Barry t k p = Barry { yabba :: p, dabba :: t k }

instance Functor (Barry a b) where
    fmap f (Barry { yabba = x, dabba = y }) = Barry { yabba = f x, dabba = y }
