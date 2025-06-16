
> import Data.Char (toUpper)
> import Data.List (span, intercalate)
> import qualified Data.Map as M (Map (..), lookup, insert, fromList)
> import Control.Monad.State 
> import Control.Monad.Writer 

  
> {-# LANGUAGE RankNTypes #-}

==========================================================================================================
                                 FUNCTIONAL PROGRAMMING  

Introductory Remarks:

In this small project I have investigated many topics within profunctor options in category theory via Haskell. There are some explanations provided before the solutions and then some GHCI output examples follow. This was part of a larger coursework in functional programming and theoretical computer science. 

==========================================================================================================
==========================================================================================================

1. Introduction to Lenses:

> data Address = A { num :: String, street :: String, town :: String } 
> instance Show Address where
>    show a = num a ++ " "++ street a ++ ", " ++ town a

> data Person = P { name :: String, address :: Address } 
> instance Show Person where
>    show :: Person -> String
>    show p = name p ++ " (" ++ show(address p) ++ ") "

> data Book = B { title :: String, protagonist , antagonist :: Person }
>    deriving Show

> bakerSt , stonyhurst :: Address
> bakerSt = A {num = "221b",street = "Baker St",town = "London"} 
> stonyhurst = A {num = "1",street = "Hodder Pl",town = "Clitheroe"}

> holmes, moriarty :: Person
> holmes=P{name="Sherlock Holmes",address=bakerSt} 
> moriarty=P{name="James Moriarty",address=stonyhurst}

> valley :: Book
> valley = B {title = "The Valley of Fear",
> protagonist = holmes, antagonist = moriarty }
  
Part 1: 

1st Law: viewing the source returns the focus and then updating the source with that focus must return the original source. This is because: u s (v s) = u s (a) = s. In simpler language, updating a source with its existing focus must not modify the source. 

This law is satified by fstLens because u (x,y) (v (x,y)) = u (x,y) x = (x,y)

2nd Law: updating a source with a focus and then viewing it, returns the focus used to update the source. This is because updating a source with a focus returns an updated source and viewing that source returns that focus used to update the source. 

This law is satified by fstLens because: 
v (u (x,y) x) = v (x,y) = x 
v (u (x,y) x') = v (x',y) = x' 
It returns the focus used to update the source. 

3rd Law: updating a source with a focus, a, will return a new source. However, updating it again with a new focus, a', is similar to simply having updated the original source with the focus a'. It is simply that updating twice is the same as updating once. 

This law is satified by fstLens because:

u (u (x,y) k) k' = u (k,y) k' = (k',y) = u (x,y) k'

> data Lens a s = Lens { view :: s -> a, update :: s -> a -> s}

> fstLens :: Lens a (a,b)
> fstLens = Lens f g where 
>    f (x,y)     = x 
>    g (x, y) x' = (x', y)

==========================================================================================================
Part 2:

Brief Explanation: addressLens is used to view and update the address of a person. cityLens is used to view and update the town in an address. protagonistLens is used to view and update the protagonist of a book. Lambda function are used to make needed updates.

> addressLens :: Lens Address Person 
> addressLens = Lens address (\(P name _) a -> P name a)

> cityLens :: Lens String Address 
> cityLens = Lens town (\(A num street _) t -> A num street t)

> protagonistLens :: Lens Person Book 
> protagonistLens = Lens protagonist (\b p -> b {protagonist = p})

GHCI EXAMPLES:

ghci> view addressLens holmes 
221b Baker St, London
ghci> let newAddress = A {num = "221b", street = "Hightgate", town = "London"}
ghci> let holmes_highgate = update addressLens holmes newAddress
ghci> print holmes_highgate 
Sherlock Holmes (221b Hightgate, London)
ghci> view cityLens bakerSt 
"London"

==========================================================================================================
Part 3: 

Brief Explanation: view function, f, takes a source integer and returns a Boolean focus. If the source is a positive integer, it returns True, else it returns False. It does it via operator >= which returns a Boolean. The update function, g, takes a source integer and updates it with a Boolean focus. If the focus is True, then the absolute value of the source Integer is returned, to ensure that the source is positive. However, if the the focus is False then the minus of that absolute value is returned to ensure that we get a negative source back. This is done via guarded equations.

> signLens :: Lens Bool Integer 
> signLens = Lens f g where 
>     f x   = x >= 0 
>     g x b | b         = abs x 
>           | otherwise = - abs x 

GHCI examples:

ghci> update signLens 2 True 
2
ghci> update signLens 2 False 
-2
ghci> update signLens 0 False 
0
ghci> update signLens (-1) False 
-1
ghci> update signLens (-1) True
1
ghci> view signLens 0
True
ghci> view signLens 5
True

==========================================================================================================
Part 4

Brief Explanation: As divLens d treats n as (q,r) = divMod n d, we know that to get the 
quotient by simply taking the 1st element of such tuple. So, the view function takes a source 
,source y here, and then applies fst to the tuple of quotient and remainder (q,r) returned as a result 
of divMod operation between the int n and the source y. This will return a focus, which is the 
quotient of the division (the first element of the tuple). 

The update function g simply takes the source y and a focus x. It, then, using some algebra 
multiplies the int n by the focus and adds the product to the result of snd (divMod y n) which 
returns the remainder part (r) of the (q,r). This is done so in order to modify the source so that it aligns with the new focus (quotient) while, at the same time, keeping the same remainder upon division by n. 

data Lens a s = Lens { view :: s -> a, update :: s -> a -> s}

> divLens :: Int -> Lens Int Int 
> divLens n = Lens f g where 
>    f y    = fst (divMod y n) 
>    g y x  = n * x + snd (divMod y n) 


GHCI Examples:

ghci> update (divLens 7) 30 5
37

view (divLens 7) 30
4

ghci> update (divLens 5) 30 0
0

ghci> update (divLens 0) 30 0
*** Exception: divide by zero

ghci> update (divLens (-2)) 30 5
-10

ghci> update (divLens (-2)) 30 (-5)
10

ghci> divMod 30 (-2)
(-15,0)

ghci> divMod (-30) (-2)
(15,0)

==========================================================================================================
Part 5:

Brief Explanation: justLens takes a Lens of any focus type (a) and a source of Maybe any type (Maybe a). 
its view function (f) should extract a focus from (Maybe a) source. If the source is Nothing, the Lens isn't well-behaved, explained below why, because it won't be able to return a focus of any type (a). So, I have decided to throw an error here. However, if the source is (Just a) type, it will extract the focus.
The update function (g) takes a source (Maybe a) which can be (Just a) or Nothing and updates it with a provided focus y. If Nothing is the source, Nothing is returned. If the source is (Just a), then the updated source, Just y, is returned.

> justLens :: Lens a (Maybe a)
> justLens = Lens f g where 
>    f (Just x)  =  x 
>    f Nothing = error "nothing entered." 
>    g Nothing _   = Nothing 
>    g (Just _) y  = Just y 

GHCI Examples:

ghci> view justLens Nothing
*** Exception: nothing entered.

ghci> update justLens Nothing 3
Nothing
ghci> update justLens (Just 4) 3
Just 3
ghci> update justLens (Just "hello") "hi"
Just "hi"
ghci> update justLens (Just "German Beer?") "Belgian Trappist Beer"
Just "Belgian Trappist Beer"

It is NOT well behaved because it violates the first two laws when the value of Maybe a is Nothing, shown below:

If s == Just x : u s (v s) = g Just x (f Just x) = g (Just x) x = Just x. LAW HOLDS as the same source is returned 
If s == Nothing : u s (v s) = g Nothing (f Nothing) = g Nothing (???) [the focus is unknow]. Also, g Nothing is Nothing no matter what focus we use. LAW VIOLATED

The second Law: v (u s a) 
If s == Just a : v (u s a) = f (g Just x y) = f (Just y) = y (HOLDS HERE as the same focus is returned)

if s == Nothing : v (u s a) = f (g Nothing y) = f (Nothing) = "???" [we have an error here. There is no source to extract its focus]. So, we don't get the focus back. We can't extract a type a from a Nothing. The second law is violated. 

==========================================================================================================
Part 6: 

Brief Explanation: headLens has focus a (any type) and source of [a] (a list of any type). Its view function, f :: [a] -> a , takes a source of type [a] and returns a focus of type a. It does that by simply taking the head of the list, using either of the methods outlined (one in comments). 
If the source is an empty list, however, it is unable to return a focus of any type (head of
empty list is undefined). 
Its update function, g :: [a] -> a -> [a], takes a source and returns an potentially updated source with a provided focus. It takes a list (empty or xs and a focus of type a and just changes the head of the source list with the new focus). However, if the source is an empty list, it will just create a singleton list with the focus provided. 

> headLens :: Lens a [a] 
> headLens = Lens f g where 
>    f []       = error "empty list provided as a source"
>    f (x:xs)   = x -- or f xs = head xs 
>    g []     y = [y]
>    g (_:xs) y = y:xs 

ghci> view headLens [1,2,3,4]
1
ghci> view headLens []
*** Exception: empty list provided as a source

ghci> update headLens [1,2,3,4] 3
[3,2,3,4]

ghci> update headLens [1,2,3,4] 1
[1,2,3,4]

ghci> update headLens [] 3
[3]

It isn't well-behaved because: The view function, f, takes a sources of type [a] and should return a focus of type a. However, when the source is an empty list, the 1st law is violated:

1st Law: u s (v s) = s => g [] (f []) = g [] ("???") 
We cannot update a source with a value we don't have because view, f, on an empty list cannot return a focus of type a. There is nothing in an empty list. Also, update, g, takea a source of type [a] with a focus of type a and returns a new source of type [a]. However, we fail to return a any value from the view function, therefore, by applying g [] ("????") we cannot get back [] 

2nd and 3rd laws hold

2nd law: if s = [] => v (u s a) = a => v (u s x) = f (g [] x) = f ([x]) = x.  HOLDS 
2nd law: if s = xs => v (u s a) = a => v (u s a) = f (g  (x:xs) y) = f (y:xs) = y. HOLDS 

3rd law: if s = [] => u (u s a) a' = u s a' => f (g [] a) a' = f ([a]) a' = [a'] Also: g [] a' = [a']. Law HOLDS 

The same reasoning can be extended if s = xs (a non-empty list)

==========================================================================================================
Part 7:

Brief Explanation: atLens takes an integer and returns a Lens. The source of the Lens is a list of any type and the focus is any type (of the same type as the source). The view function, f :: [a] -> a, if applied to the source, a list, checks the length of the source and only returns a focus (element at that 
index) if the length of the source is greater than the index provided. Otherwise, I have decided to throw an error as there is no element if the index passes the list. This also handles the situations the 
list is empty. The update function, g :: [a] -> a -> [a], takes a source (xs) and a focus (y) and then checks again if the length is greater than the index provided. If so, it will take n elements from the source xs, creates a singleton list from the focus, and drops n+1 elements from the source xs before appending them all, effectively potentially altering the source by replacing the element at index n of the source with the focus provided. For example: 

take 2 [1,2,3,4,5] ++ [7] ++ drop (2+1) [1,2,3,4,5] = [1,2,7,4,5] assuming focus, y, is 7 and n is 2. 
take 2 [1,3,3,4,5] ++ [3] ++ drop (2+1) [1,3,3,4,5] = [1,3] ++ [3] ++ [4,5] = [1,3,3,4,5] assuming focus, y, is 3 and n is 2 (this doesn't change the list)

> atLens :: Int -> Lens a [a]
> atLens n = Lens f g where 
>    f xs | length xs > n   = xs !! n
>         | otherwise       = error "the list is empty or its length is less than the index n" 
>    g xs y | length xs > n = take n xs ++ [y] ++ drop (n+1) xs 
>           | otherwise     = error "empty list or short list"

ghci> update (atLens 2) "abcd" 'x'
"abxd"

ghci> update (atLens 2) "" 'x'
"*** Exception: empty list or short list"

ghci> view (atLens 2) [1,2,3,4]
3

ghci> view (atLens (-2)) [1,2,3,4]
*** Exception: Prelude.!!: negative index

ghci> view (atLens 3) [1,2,3,4]
4

ghci> view (atLens 4) [1,2,3,4]
*** Exception: the list is empty or its length is less than the index n

ghci> view (atLens 0) []
*** Exception: the list is empty or its length is less than the index n

It isn't well behaved because if the source is an empty list or if the source's length is smaller than the index provided, we cannot index into it so atLens will not succeed. 

1st Law: (if source is an empty list []) u s (v s) = s : g [] (f []) = g [] ("????") \= []
{ we cannot extract a focus from an empty list as a source}. Therefore, updating a source with an unknown focus doesn't return the source. 1st Law is violated. 

==========================================================================================================
Part 8: 

Brief Explanations: 

This function takes a key and returns a Lens with Source a (map) and focus a (Maybe a). The view function simply looks up the value in the source map using lookup function and returns the focus (in forms of Maybe value) if it exists (see the examples below).If it exists, it returns Just x. Otherwise, it returns Nothing. The update function, g, works as follows: It updates a source map (m) with a (Maybe a) focus. If the focus is Nothing, it then only returns the map in its original form. If, however, the focus is a (Just y), it inserts (or replaces) the y in the map (m) at the corresponding x key. If the key exists, it replaces its value with y. If it doesn't, it creates key x and inserts value y there, essentially extending the map. 

> lookupLens :: Ord k => k -> Lens (Maybe a) (M.Map k a)
> lookupLens x = Lens f g where 
>     f  = M.lookup x
>     g m Nothing  = m 
>     g m (Just y) = M.insert x y m    

GHCI examples:

In this Part I have utilized fromList from map library to create a map for GHCi examples 

ghci> let my_map' = M.fromList [("FPR", "J. Gibbons"), ("SEM", "A. Simpson"), ("QUC", "S. Gogioso")]

ghci> print my_map' 
fromList [("FPR","J. Gibbons"),("QUC","S. Gogioso"),("SEM","A. Simpson")]

ghci> (view $ lookupLens "SEM") my_map' 
Just "A. Simpson"

ghci> (update $ lookupLens "SEM") my_map' Nothing 
fromList [("FPR","J. Gibbons"),("QUC","S. Gogioso"),("SEM","A. Simpson")]

ghci> (view $ lookupLens "DAT") my_map' 
Nothing

% ghci> (update $ lookupLens "ALG") my_map' (Just "R. Hinze")
% fromList [("ALG","R. Hinze"),("FPR","J. Gibbons"),("QUC","S. Gogioso"),("SEM","A. Simpson")] 

ghci> (update $ lookupLens "ALG") my_map' Nothing 
fromList [("FPR","J. Gibbons"),("QUC","S. Gogioso"),("SEM","A. Simpson")]
==========================================================================================================
Part 9: 

View Function f takes a source of type String and returns its focus of type [String]. 
The library function lines does just that, it creates a list of String from originally provided String. 
update function, g, modifies a source string with a provided focus. First, it stores the length of current source string in a variable called orig_len. Then, it takes orig_len number of elements from xss (the focus). It also uses repeat "" to ensure the focus matches the source's line count if needed. Finally, using prelude functions unlines and init, it processes the first n-1 elements from the list and flattens them with new lines interleaved, essentially creating a string. Lastly, it concatenates this string with the "untouched" last element of the focus list. 

> linesLens :: Lens [String] String 
> linesLens = Lens f g where 
>    f        = lines 
>    g xs xss = let 
>         orign_len = length (lines xs)
>         new = take orign_len (xss ++ repeat "") in unlines (init new) ++ last new 

GHCI Examples: (I had to remove GHCI outputs to be able to convert to PDF. It was an issue converting: \)


==========================================================================================================
Part 10:

Brief Explanation: 

This function takes a Lens with its view function (f :: s -> a) and its update function (g :: s -> a -> s) and a modifying function h and the source x. The view function, f, extracts the focus of the source x. The focus is fed to modifying function, (h :: a -> a), which will, "potentially", modify it, and return a new focus. This potentially modified focus is then fed to update function (g) that uses it to potentially update the source and return it. 

> modifyLens :: Lens a s -> (a -> a) -> (s -> s)
> modifyLens (Lens f g) h x = g x (h (f x))

GHCI examples:

ghci> modifyLens fstLens (+10) (3,4)
(13,4)

ghci> modifyLens (atLens 3) (^2) [1,2,3,4,5,6]
[1,2,3,16,5,6]

ghci> modifyLens justLens (^2) (Just 3)
Just 9

ghci>  modifyLens justLens (^2) Nothing 
Nothing

==========================================================================================================
Part 11:

Brief Explanation: the composed Lens l'' has view function, (f'':: s -> b) and update function (g'' :: s -> b -> s). So we need to create these two functions via the Lenses l and l', making sure the types match. Lens l' has the same source type as l''. So by applying (f' :: s -> a)to s we extract an intermediate focus of (type a) which is of the same type as the source of Lens l. So, by using the view function, (f :: a -> b), of Lens l we get the focus of type b which is the same focus as that of the composed Lens l''. 

Update function g'' takes a source of type s and a focus of type b, returning a potentially updated source of type s (g'' :: s -> b -> s). By applying f' to s, we extract an intermediate focus of (type a). This is the same as type as the source of Lens l. So, update function of Lens l, (g :: a -> b -> a) is used to update this intermediate source of (type a) with focus of type b, same type as focus of composed Lens l'', to return a potentially updated intermediate source of type a, which is the same type as the focus for Lens l'. Finally, update function of l', (g' :: s -> a -> s), is used on on the source s with the focus of type a to give us the final output, exactly what is expected of g'' s b. This chaining operation is similar to function composition. 

data Lens a s = Lens { view :: s -> a, update :: s -> a -> s}

> composeLens :: Lens b a -> Lens a s -> Lens b s 
> composeLens (Lens f g) (Lens f' g') = Lens f'' g'' where 
>    f'' s = f (f' s)
>    g'' s b = g' s (g (f' s) b)

GHCI Examples: 

ghci> view (composeLens cityLens addressLens) holmes
"London"

ghci> view (composeLens cityLens addressLens) moriarty
"Clitheroe"

ghci> update (composeLens cityLens addressLens) holmes "Pluto"
Sherlock Holmes (221b Baker St, Pluto) 

==========================================================================================================
Part 12:

Brief Explanation: The view function, f, takes a source of type ((a,b), c) and just focuses on a. It is done via simply extracting the 1st element of a nested pair. 
The update function, g, simply takes a source of type ((a,b), c) and a focus of type a and updates the source with the new focus. I have used x' as a focus to update the Lens of ((_, y), z) to ((x',y), z)

data Lens a s = Lens { view :: s -> a, update :: s -> a -> s}

> fstFstLens :: Lens a ((a,b), c)
> fstFstLens = Lens f g where 
>    f ((x,_), _)   = x
>    g ((_,y), z) x' = ((x',y), z)

GHCI Examples: The examples focus on Baroque era composers of classical music.

ghci> view fstFstLens (("Johann Sebastian Bach", "Classical"), "Germany")
"Johann Sebastian Bach"

ghci> update fstFstLens (("Johann Sebastian Bach", "Classical"), "Germany") "George Frideric Handel"
(("George Frideric Handel","Classical"),"Germany")

==========================================================================================================
Part 13:

Brief Explanation: This idLens takes a Lens with a view function f and update function g. Its view function, f, simply returns the existing focus, whereas its update function g, regardless of the focus given, will just return the original source. 

> idLens :: Lens a a 
> idLens = Lens f g where 
>    f     = id 
>    g s _ = s 

Alternatively using function composition with id as the view function, f, and const as the update function, g. const will ignore the focus and will always return the source untouched. 

> idLens' :: Lens a a 
> idLens' = Lens id const

GHCI examples: 

ghci> view idLens [1,2,3]
[1,2,3]
ghci> update idLens [1,2,3] [4,5]
[1,2,3]
ghci> update idLens ["Hello"] ["hi"]
["Hello"]

It is related to composeLens in such that it boils down to function composition laws in Haskell and in math overall such that composing any function with identity function returns the function itself. 
(f . id = f) and (id . f = f )
The same is extended to Lenses. Composing any Lens with the idLens will return the Lens untouched. For example: composeLens (Lens a s) idLens = Lens a s. For example:

ghci> view (composeLens addressLens idLens) holmes
221b Baker St, London

ghci> view addressLens holmes 
221b Baker St, London

==========================================================================================================
Part 14: 

Brief Explanation: pairLens takes Lenses l and l' with their view and update function as f, g and f', g' respectively. It creates another Lens whose focus is a tuple of the same type as focuses of l and l' and whose source is a tuple of a type similar to sources of l and l'. The view function of l'', f'' is applied to the source of type (s,t) to extract a focus of type (a,b). It can do that by calling (f s, f' t) which with extract a and b respectively since: (f:: s -> a) and (f' :: t -> b). 
The update function, g'', takes a source of type (s,t) and attemps to update it with a focus of type (a,b), (x',y') in the function definition, to return a potentially updated source of type (s,t). It can do that by calling (g s x', g' t y'). That way, g attempts to update source s with focus x' while g' attempts to update its source t with new focus y'. It type checks since (g :: s -> a -> s) and (g' :: t -> b -> t)

f'' :: (s, t) -> (a,b)
g'' :: (s,t) -> (a,b) -> (s,t)

> pairLens :: Lens a s -> Lens b t -> Lens (a,b)(s,t)
> pairLens (Lens f g) (Lens f' g') = Lens f'' g'' where 
>    f'' (s,t) = (f s, f' t) 
>    g'' (s,t) (x', y') = (g s x', g' t y')  

GHCI Examples: 

ghci> let my_len = pairLens (composeLens cityLens addressLens) (composeLens cityLens addressLens)

ghci> view my_len (holmes, moriarty) 
("London","Clitheroe")

ghci> update my_len (holmes, moriarty) ("Liverpool", "Dublin")
(Sherlock Holmes (221b Baker St, Liverpool) ,James Moriarty (1 Hodder Pl, Dublin) )

==========================================================================================================
Part 15: 

Brief Explanation: The view function f applied to any source, returns a unit type () focus. The update function g applied to a source s with focus of unit type will just return the source. 

> unitLens :: Lens () s 
> unitLens = Lens f g where 
>    f _    = ()
>    g s () = s 

GHCI Examples: 

ghci> view unitLens ("ABC", ["DEF"])
()

ghci> update unitLens ("ABC", ["DEF"]) ()
("ABC",["DEF"])

How does this relate to pairLens, fstLens, and fstFstLens?

unitLens provides a trivial view of the sources. It doesn't update the source. This is because unitLens' focus is the unit (). fstLens works on a first component of a source pair. The view function, f, views the first component and the update function, g, changes that first component of the source pair with a new focus.  Similar to fstLens, fstFstLens focuses on the first element of a pair but extends that to a nested pair. pairLens allows us to focus on both sources and focuses of two separate lenses, essenially creating a new Lens whose focus is the focus of two lenses combined and whose source is the sources of the two lenses combined. In other words, as we go unwards from unitLens, we increase in complexity of operations. unitLens is the most trivial form of operation (trivial view of the source) that has no data manipulation capabilities. fstLens and fstFstLens, however, allow us to perform more complicated operations, with pairLens going a step further and pairing two lenses for potentially parallel operations.

==========================================================================================================
Part 16: 

Brief Explanation: eitherLens constructs a new lens from two lenses, with different sources, the source of the lens created with eitherLens can be the source of the first lens or that of the second lens. 

If in Either (x y), the Left constructor is involved and Left (x) is picked as the source of the resulting Lens, then the view function of the resulting Lens, f'', just calls the view function of the first lens, f, to extra the focus of (type a) since the Left constructor of the Either, in this example, just picks the source that is of the same type as that of the first Lens. This will return the focus. Conversely, if the Right (y) is picked, then view function, f', of the second Lens is called to extract the focus, again of (type a). 

Similar logic concerns update, g'', function. If Left constructor is invoked and Left (x) is picked as the source, we update that with a new focus x' which will call update function of 1st lens, g, on x with x' and then wraps Left around the results for type correctness with the source of the resulting Lens. Likewise, if Right (y) is picked, g' is called on y with a new focus y', returning an updated source, finally this updated source is wrapped with Right for type correctness. 

> eitherLens :: Lens a b -> Lens a c -> Lens a (Either b c) 
> eitherLens (Lens f g) (Lens f' g') = Lens f'' g'' where 
>      f'' (Left x)     = f x 
>      f'' (Right y)    = f' y
>      g'' (Left x) x'  = Left (g x x')
>      g'' (Right y) y' = Right (g' y y')

GHCI EXAMPLES:

ghci> :t fstLens
fstLens :: Lens a (a, b)
ghci> :t fstFstLens 
fstFstLens :: Lens a ((a, b), c)
ghci> let eithRes = eitherLens fstLens fstFstLens
ghci> :t eithRes 
eithRes :: Lens a (Either (a, b1) ((a, b2), c))
==========================================================================================================
==========================================================================================================

PART 2. Prisms  

> data Prism a s = Prism {match :: s -> Either s a, promote :: a -> s}

> maybePrism :: Prism a (Maybe a) 
> maybePrism = Prism m p where 
>     m (Just x) = Right x 
>     m Nothing  = Left Nothing 
>     p y        = Just y 


Part 17: 

Prism Laws:

1. m (p b) = Right b 
2. m a     = Right b => p b = a 
3. m a     = Left a' => Left a 

(I have used "extract a focus" and "coerce a source to a focus" or "coerce a source" interchangeably in the definitions, all meaning the same that the match function takes a source and successfully returns its focus or coerces the source to the focus.)


Law 1: promoting a focus of type b to a source and then matching that source shall successfully extract the focus of type b.  
Law 2: Successfully coercing the source to the focus of type b, via match, implies that promoting the focus of type b results in source of type a. (in simpler words, if match successfully extracts a focus from a source, then we should be able to reconstruct the original source from that focus)
Law 3: Matching a source of type a resulting in failure to coerce to the focus, instead returning a source of type a' implies that a' equals a (in simpler words, failing to extract a focus from source, should leave the source untouched.)

maybePrism is well behaved because:
Law 1: m (p a) = m (Just x) = Right x. (According to function definition). LAW HOLDS
Law 2: m (Just x) = Right x (successfully coerced source to focus) -> p (x) = Just x (According to function definition). LAW HOLDS 
Law 3: m (Nothing) = Left Nothing' (failed to coerce source to focus) -> m (Nothing') = Left Nothing (in the function m Nothing = Left Nothing). There has been no change in source (Nothing). LAW HOLDS 

==========================================================================================================
Part 18: 

Brief Explanation: The match function, f, takes a source of type (Either a b). If it receives Left x (Left constructor gets invoked), it is of (type a) so it can successfully coerce it to a focus as the focus is also of (type a). It successfully extracts the focus x and wraps Right around it for type correctness. However, if it receives a Right y (of type b it cannot extract a focus from), it just returns the source (Right y) and wraps Left around it. 

The promote function, g, takes a focus and turns it into a source using Left constructor, which deals with the correct focus of type a (the focus in function signature)

data Prism a s = Prism {match :: s -> Either s a, promote :: a -> s}

> leftPrism :: Prism a (Either a b) 
> leftPrism = Prism f g where 
>    f (Left x)  = Right x 
>    f (Right y) = Left (Right y)  
>    g           = Left   

GHCI Examples 

ghci> match leftPrism (Left "Haskell is awesome")
Right "Haskell is awesome"

ghci> match leftPrism (Right "This will fail to coerce")
Left (Right "This will fail to coerce")

==========================================================================================================
Part 19 

Brief Explanations: The match function takes a double, d, calls properFraction on it which returns a tuple (x,y). x representing an integral number and y representing a fraction with absolute value less than 1. It then checks if y == 0. If so, d represents an integer since the fraction, y, equals zero. For example, properFraction 3.0 = (3, 0.0). In this case, the match function, then coerces d into an integer (the focus) by just simply taking the integral part of properFraction, x, and wrapping a Right around it. Otherwise, it will fail to coerce the source double to the focus integer. It will just wrap a Left around the source, d, and returns that. The promote function, p, takes a focus of type Integer, n, and calls fromIntegral to convert into source of type Double. 

data Prism a s = Prism {match :: s -> Either s a, promote :: a -> s}

p :: Integer -> Double 
m :: Double -> Either Double Integer

> wholePrism :: Prism Integer Double 
> wholePrism = Prism m p where 
>    m d | y == 0    = Right x 
>        | otherwise = Left d
>        where (x,y) = properFraction d
>    p               = fromIntegral 


GHCI examples:
ghci> let tryDiv3 = either id (promote wholePrism . (`div` 3)) . match wholePrism 
ghci> tryDiv3 6.0
2.0
ghci> tryDiv3 6.5
6.5
ghci> tryDiv3 9.5
9.5
ghci> tryDiv3 9.0
3.0
==========================================================================================================

Part 20: (SEE REFERENCE IN APPENDIX: [2])

Prisms are dual to Lenses in a way that a prism from (s,t) to (a,b) is similar to a Lens from (t,s) to (b,a) in the opposite categories. In addition, Lenses are seen as product types such as tuples whereas Prisms work with sum types (Either). Prisms allow for failure so that if a view operation isn't successful, the source data structure is returned, because they support sum types. Most importantly, looking at their type signatures, we can see that: view :: s -> a and update :: s -> a -> s . Lens is a way into the data structure. However, regarding prisms: match :: s -> Either s a and promote :: a -> s. Promote function allows for a reverse arrow operation, the exact opposite of view function in Lens. Also, since prisms allow going from source to focus and back to source (if match fails) as well as from focus to source using promote, they are seen as bidirectional data structures. Lenses are, however, single directional, from source to focus or from source to source, not the other way around. 

==========================================================================================================
Part 21: 

Brief Explanatoon: maybePrism' has a downcast function, f, which takes a source of (Maybe a) and returns (Maybe a). If the source is a Nothing it just returns Nothing. If the source is (Just a) it returns a (Just a). Quite trivial. The upcast operation, g, takes any data of type a and upcasts it to a source of Just a. 
f :: Maybe a -> Maybe a 
g :: a -> Maybe a

> data SimplePrism a s = SimplePrism {downcast :: s -> Maybe a, upcast :: a -> s}

> maybePrism' :: SimplePrism a (Maybe a)
> maybePrism' = SimplePrism f g where 
>       f (Just x) = Just x 
>       f Nothing  = Nothing 
>       g          = Just 

Regarding leftPrims', the downcast function, f, takes a source of type (Either a b) and returns its focus of type (Maybe a). If (Left x) is given to the downcast function, f, then it is of correct type :: a and it will convert it to a (Maybe a) by wrapping Just around x. However, if Right y is given to downcast function, f, it won't be able to turn it into a Maybe a since it is of incorrect type b. Hence, it will return Nothing. The upcast function, g, takes a focus of type :: a and turns it into a source of type Either by wrapping Left over it. 

f :: Either a b -> Maybe a 
g :: a -> Either a b 

> leftPrism' :: SimplePrism a (Either a b)
> leftPrism' = SimplePrism f g where 
>       f (Left x) = Just x
>       f (Right y) = Nothing
>       g           = Left 


SimplePrism Laws: 

d (u b) = Just b (upcasting a focus to a source and then downcasting this source sequentially must return the original focus )
d a = Just b => u b = a (if successfully downcasting a source to a focus, then the upcast operation on that focus must reconstruct the original source). 
d a = Nothing => a (failed downcast operations on source means there is no valid focus in source. Hence, this operation must not alter the source)
d (u b) /= Nothing (downcasting an upcasted focus must not fail because the focus already exists)


GHCI EXAMPLES: 

ghci> downcast leftPrism' (Left "Haskell is awesome")
Just "Haskell is awesome"
ghci> upcast leftPrism' 3.5
Left 3.5
ghci> downcast maybePrism' (Just 5)
Just 5
ghci> downcast maybePrism' Nothing
Nothing

==========================================================================================================
Part 22:

Brief Explanations:

By applying the match function, m to source s, we obtain an Either type with Left constructor returning  the source s unchanged and Right constructor returning the focus x (of type a), coercing the source. I have used case expression to handle the alterative returns of m s, each of which follows a different path. If the Right is picked (Right x) of type a, the modifying function, h :: a -> a, potentially modifies the focus and then the promote function p creates a potentially modified source from the result of h x. However, if the Left is picked (Left s) of type s, then, the modifying function, h, cannot modify it. So, we simply return the source. The source is left untouched and propagated through since the modifying function h :: a -> a. It doesn't apply to source type; it only applies to focus type. 

> modifyPrism :: Prism a s -> (a -> a) -> (s -> s)
> modifyPrism (Prism m p) h s = case m s of
>                    Right x -> p (h x)
>                    Left s -> s

==========================================================================================================
Part 23: 

Brief Explanation: composePrism takes two prisms, p and p', and returns a composed prism. Using nested cases, we can first check for the results of first Prism's match operation on source s: (f s). If it fails to coerce (extract the focus), the Left constructor is envoked which will return the source untouched and propagated through since it isn't of a type Prism p' can work on. However, if it succeeds to coerce the source to the focus (extract the focus) then Right constructor is invoked which will return the focus. This focus is now the source of the 2nd Prism p'. Again, we need to check if the match function of 2nd prism, p', can coerce this source and extract its focus using cases. If yes, then Right constructor is envoked which will return the focus of type b, the same type as the focus of the composed Lens. However, if it fails to coerce, we just propogate the failure and return the original source s. 

> composePrism :: Prism a s -> Prism b a -> Prism b s
> composePrism (Prism f g) (Prism f' g') = Prism f'' g'' where
>   f'' s = case f s of
>       Right a -> case f' a of
>            Right b -> Right b   
>            Left _ -> Left s      
>       Left _ -> Left s         
>   g'' b = g (g' b)

Or as we see a repeat pattern of cases, we can utilize monadic do notation and return operations with similar explanations as above. 

> composePrism' :: Prism a s -> Prism b a -> Prism b s
> composePrism' (Prism f g) (Prism f' g') = Prism f'' g'' where
>     f'' s = do
>          a <- either (const (Left s)) Right (f s) 
>          b <- either (const (Left s)) Right (f' a) 
>          return b 
>     g'' b = g (g' b) 


GHCI Examples:

ghci> let composed' = composePrism leftPrism maybePrism
ghci> let composed = composePrism' leftPrism maybePrism

ghci> match composed' (Left (Just 17)) 
Right 17
ghci> match composed' (Left Nothing) 
Left (Left Nothing)

ghci> match composed (Left Nothing) 
Left (Left Nothing)

==========================================================================================================
==========================================================================================================
Part 3. Polymorphic

> data PolyLens a b s t = PolyLens { pview :: s -> a, pupdate :: s -> b -> t}

PolyLens A B S T provides access to an A focus within an S source, but also allows one to update the focus to a different type B, which will update the source to a different type T (presumably dependent on B). 

> fstPolyLens :: PolyLens a a' (a,b) (a',b) 
> fstPolyLens = PolyLens v u where 
>    v (x,y)    = x 
>    u (x,y) x' = (x',y)

Part 24: 

The Lenses with general polymorphic versions are (my assumption here is that the current Lenses in their existing forms can be altered slighly, accepting another focus type to allow changing the source, to become instances of the Polymorphic Lens explained in this section and not that we can freely change function signature to make them polymorphic):

1. justLens because it takes a source of Maybe any type and a focus of any type. This can be slightly altered so that it can alter the source to (Maybe b) by acceping a focus of type b adhere to PolyLens type signature. 
2. headLens because it takes any type as focus and a list of any type as source. This can also be slightly altered so that it can modify the source, from a list of type a to a list of type b by accepting another focus of type b to adhere to PolyLens type signature. 
3. atLens: similar logic and explanation as above.
4. fstFstLens because it can be generalized to any form and similar explanation as above
5. eitherLens: Could be altered slighly to modify the type within Either through changing focus types. such as transforming an Either a b to an Either e f. 
7. pairLens 

==========================================================================================================
Part 25: 

data PolyLens a b s t = PolyLens { pview :: s -> a, pupdate :: s -> b -> t}

1. pu s (pv s) = s (pviewing a source returns its focus and then pupdating that source with its focus must just return the original source)

2. pv (pu s a) = a (pviewing a source after pupdating a source with a focus of type a must return the focus used to pupdate the source)

3. pu (pu s a) a' = pu s a' (pupdating a source with a focus of type a and then pupdating it again with a focus of type a' is the same as pudating it initially with the focus of type a')

The Lens laws stay the same as explained for Lenses in Section 1.
==========================================================================================================

Part 26: 

Brief Explanation: modifyPolyLens takes a PolyLens with its pview and pupdate functions f and g respectively, a modifying function, h :: a -> b, and a source, s, and returns a new source, t. It does it by first applying pview to the source to get its focus of type a. We need the focus of type a since the modifying function h :: a -> b. It takes a type (a) and outputs a type (b). In other words, b is the output of applying the modifying function h to original focus a. This output is then used in pupdate function, g, to modify the source using (g s b) operation to create a potentially modified source of type t. 

> modifyPolyLens :: PolyLens a b s t -> (a -> b) -> (s -> t)
> modifyPolyLens (PolyLens f g) h s = g s (h (f s))

type checking: 

f :: s -> a 
g :: s -> b -> t 
h :: a -> b 

f s :: a 
h (f s) :: b 
g s b :: t 

GHCi examples: 

ghci> let modified_res = modifyPolyLens fstPolyLens (\ x -> x^2) (1,2)
ghci> modified_res 
(1,2)

ghci> let modified_res' = modifyPolyLens fstPolyLens (\ x -> x^2) (2,1)
ghci> modified_res' 
(4,1)

==========================================================================================================
Part 27:

Brief Explanations: modifyPolyPrism takes a PolyPrism, a modifying function h :: a -> b that will act on the source s to potentially modify it and return a source of type t. As the modifying function h :: a -> b requires a (type a)as input, by applying the pmatch function f to the source s we get back an Either type (the existing focus of the source s or a new source of type t). If Left constructor is invoked, the source propagates through without being impacted by modifying function h. However, if Right constructor is invoked, the focus (a) of source s is returned and passed down to the modifying function h, returning an updated focus of type b. This is passed to ppromote function g that will create a new source of type t. 

> data PolyPrism a b s t = PolyPrism{ pmatch :: s -> Either t a, 
>                                    ppromote :: b -> t}

> modifyPolyPrism :: PolyPrism a b s t -> (a -> b) -> (s -> t)
> modifyPolyPrism (PolyPrism f g) h s = case f s of 
>         Right x -> g (h x)
>         Left  t -> t 

==========================================================================================================
==========================================================================================================
Part 4. Traversals

> traverseTree :: Monad m => (a -> m b) -> (Tree a -> m (Tree b))
> traverseTree f (Tip x)   = do 
>                            y <- f x -- if f x doesn't fail 
>                            return (Tip y) -- pure 
> traverseTree f (Bin t u) = do 
>                              t' <- traverseTree f t 
>                              u' <- traverseTree f u 
>                              return (Bin t' u')

> printAndParity :: Int -> IO Bool 
> printAndParity n = do {print n; return (odd n)}

> class Traversable g where 
>   traverse :: Applicative f => (a -> f b) -> (g a -> f (g b)) 

> data Tree a = Tip a | Bin (Tree a) (Tree a) deriving Show
> t :: Tree Int
> t = Bin (Tip 1) (Bin (Tip 2) (Tip 3))

> t'' = Bin (Bin(Tip 1)(Tip 2)) (Bin (Tip 3)(Tip 4))

> t' :: Tree Char 
> t' = Bin (Tip 'a')(Bin (Bin( Tip 'b')(Tip 'c'))(Bin (Tip 'd')(Tip 'e')))

> t''' :: Tree Bool 
> t''' = Bin (Tip False)(Bin (Bin( Tip False)(Tip True))(Bin (Tip False)(Tip True)))


Part 28. 

Brief Explanations: contents takes a Tree of type a and returns the contents of the tree in a list format. It first checks the Tip constructor of the tree, creating a singleton list with its value and returning it. Then, it recursively calls on left and right subtrees and appends the results of the calls.  

> contents :: Tree a -> [a]
> contents (Tip x) = [x]
> contents (Bin l r) = contents l ++ contents r

GHCI Examples:

ghci> contents t
[1,2,3]
ghci> contents t'
"abcde"
ghci> contents t''
[1,2,3,4]

==========================================================================================================
Part 29:

Brief Explanation: This function takes a Tree of type a and a list of type b [b] that is used to change the contents of the tree to type b. It uses an auxiliary function called auxFunc that creates a tuple containing of the Tree of type b and the list of leftover elements. 

Definition of auxFunc: This function takes a Tree and a list of type b (for labels) and returns the labelled Tree as well as the leftover labels (elements) in a tuple. Applying auxFunc to the Tip of the tree with a list of labels (y:ys) will just change the label of the Tip to y and return the rest of the labels, ys: (Tip y, ys). 

Applying auxFunc to the subtrees l r (Bin l r) with labels ys will first traverse the left subtree and labels it while retaining the leftover labels which are then used to label the right subtree. At the end, a tuple containing the newly labelled tree and the leftover labels is returned and passed to label function which will extract the labelled tree element of the tuple using Prelude function fst. 


> label :: Tree a -> [b] -> Tree b
> label tree bs = fst (auxFunc tree bs) 

> auxFunc :: Tree a -> [b] -> (Tree b, [b])
> auxFunc (Tip _) (y:ys) = (Tip y, ys)  
> auxFunc (Bin l r) ys =
>    let (l', rems) = auxFunc l ys  
>        (r', m_rems) = auxFunc r rems  
>    in  (Bin l' r', m_rems)  
> auxFunc _ [] = error "no more labels left."


GHCI examples:

ghci> label t' [1,2,3,4,5]
Bin (Tip 1) (Bin (Bin (Tip 2) (Tip 3)) (Bin (Tip 4) (Tip 5)))

ghci> label t' [1,2,3,4]
Bin (Tip 1) (Bin (Bin (Tip 2) (Tip 3)) (Bin (Tip 4) *** Exception: no more labels left.

ghci> label t "abcd"
Bin (Tip 'a') (Bin (Tip 'b') (Tip 'c'))

ghci> label t "abcde"
Bin (Tip 'a') (Bin (Tip 'b') (Tip 'c'))

ghci> label t''' "abcde"
Bin (Tip 'a') (Bin (Bin (Tip 'b') (Tip 'c')) (Bin (Tip 'd') (Tip 'e')))

==========================================================================================================
Part 30:

Brief Explanation: treeList's view function, f, takes a source and returns its focus. The source is a (Tree a) and the focus is the list of elements of the tree [a]. Our already defined contents function does just that. The update function, g, takes a sources of type (Tree a) and updates that with a focus of type [a] to return a potentially updated source of type (Tree a). This can be achieved via our label function. 

> treeList :: Lens [a] (Tree a)
> treeList = Lens f g where 
>     f = contents
>     g = label

GHCI examples: 

ghci> view treeList t
[1,2,3]

ghci> update treeList t [4,5,6]
Bin (Tip 4) (Bin (Tip 5) (Tip 6))

==========================================================================================================
Part 31:

Unfortunately, I failed to answer this Part. I need to work on Monads a bit more to fully capture the concepts so would really appreciate some feedback on this particular Part. I have made an attempt at solving contents again, see below, but this assumes Tree data structure, not any Traversable, which is wrong. 

> contents' :: Tree a -> IO [a]
> contents' (Bin t u) = do
>    l_t <- contents' t 
>    l_u <- contents' u 
>    return (l_t ++ l_u)   
> contents' (Tip x)   = return [x] -- pure 

GHCI EXAMPLES:

ghci> contents' t'
"abcde"

ghci> contents' t''
[1,2,3,4]

ghci> contents' t'''
[False,False,True,False,True]

ghci> contents' t
[1,2,3]

==========================================================================================================
Part 32:

Brief Explanation: To convert between two Lenses using a single function, Either has been used to provide the option to have either lens type as source and either lens type as target. If Left constructor is invoked, the LensTuple is selected with its single viewUpdate function, h. The function acts on this LensTuple and returns a Lens via invoking the Left constructor of the return singature. The library function fst is applied to the result of (h s) to extract the focus. h :: s -> (a, a -> s). fst h s = a
Likewise, the library function snd is used to extra the function (a -> s )effectively mimicking the update function g :: s -> a -> s. 

In the other direction, by invoking Right constructor, the function takes a Lens and returns a LensTuple whose viewUpdate (h) is constructed via applying f s to extract the focus (a) and g s to extra the function a -> s. Finally, they are wrapped around in a tuple. 

> data LensTuple a s = LensTuple {viewUpdate :: s -> (a,a -> s)}

> convert' :: Either (LensTuple a s) (Lens a s) -> Either (Lens a s) (LensTuple a s)
> convert' (Left (LensTuple h)) = Left (Lens f g) where
>    f s = fst (h s) -- h s = (a, a -> s) -- fst hs = a 
>    g s = snd (h s) -- snd (h s) = a -> s 
> convert' (Right (Lens f g)) = Right (LensTuple h) where 
>    h s = (f s, g s)

GHCI Examples:

ghci> let my_len = pairLens (composeLens cityLens addressLens) (composeLens cityLens addressLens)
ghci> let convertedToLens = convert' (Right my_len)
ghci> :t convertedToLens
convertedToLens
  :: Either
       (Lens (String, String) (Person, Person))
       (LensTuple (String, String) (Person, Person))

==========================================================================================================
==========================================================================================================
PART 5. Functional Representation:

> type LensVL a b s t = forall f . Functor f => (a -> f b) -> (s -> f t)
> data Identity a = Identity {unIdentity :: a}
> data Const b a  = Const {unConst :: b}


class Functor' for where 
   fmap :: (a -> b) -> (f a -> f b)

instance Functor [] where 
    fmap = map 

Function Laws:

1. Functional Composition: fmap(g . f) = fmap g . fmap f 
2. Identity :              fmap id     = id 

instance Functor Maybe where 
    fmap f (Just x)  = Just (f x)
    fmap f (Nothing) = Nothing


type LensVL a b s t = forall f . Functor f => (a -> f b) -> (s -> f t)
data Identity a = Identity {unIdentity :: a}
data Const b a  = Const {unConst :: b}

Part 33: 

Functor Law proofs for lists (SEE REFERENCE IN APPENDIX: [6]): 

instance Functor [] where 
   fmap f []  = [] 
   fmap f (x:xs) = fmap f xs ++ [f x]

Proof by induction: 

1. Proof of the 2nd law (Identity): 

fmap id xs = id xs {definition of id} = xs 

Base Case: (Empty List):

fmap id [] = {definition of fmap} [] 

Inductive Step: Assume fmap id xs = xs => prove fmap id (x:xs) = (x:xs) 

Inductive Hypothesis: fmap id xs = xs

fmap id (x:xs) = {from definition of fmap} id x : fmap id xs {from Inductive Hypothesis} = id x : xs {applying id} 
x : xs END OF PROOF 

/======================================/

2. Proof of 1st law (Function Composition)

fmap (g . f) xs = (fmap g . fmap f) xs  

Base Case: Empty List

fmap (g . f) [] = {from definition of fmap} [] = {unapplying fmap} fmap g [] = {unapply fmap} fmap g (fmap f [])

Inductive Step: 

Inductive Hypothesis: fmap (g . f) xs = (fmap g . fmap f) xs {function composition} fmap g (fmap f xs)

fmap (g . f) (x:xs) = {from definition of fmap} (g . f) x : fmap (g . f) xs = {function composition} g ( f x) : fmap ( g . f) xs = {from inductive hypothesis} g (f x) : fmap g (fmap f xs) = {unapplying fmap} fmap g (f x : fmap f xs) = {unapplying fmap} fmap g (fmap f (x:xs)) END OF PROOF 

/========================================================================================================/

Proof of Functor Laws for Maybe functor:

1. Proof of fmap id = id 

Case 1: Nothing : 

fmap id Nothing = {definition of fmap} id Nothing = Nothing 

Case 2 : Just x 

fmap id (Just x) = {definition of fmap} Just id x = {applying id} Just x. END OF PROOF 

/======================================/

2. Proof of function composition: fmap(g . f) Nothing = (fmap g . fmap f) Nothing = {definition of function composition} fmap g (fmap f Nothing) 

Case 1: Nothing : fmap (g . f) Nothing = (fmap g . fmap f) Nothing. 

fmap (g . f) Nothing = {definition of fmap} Nothing = {unapplying fmap} fmap f Nothing = {unapplying fmap} fmap g (fmap f Nothing) 

Case 2: Just x : fmap (g . f) (Just x) = (fmap g . fmap f) Just x  

First of side of the equality: fmap (g . f) Just x = {definition of fmap} Just (g . f) x = {applying function composition} Just (g (f x))

Now the other side of the equality: (fmap g . fmap f) Just x = {definition of function composition} fmap g (fmap f Just x) = {definition of fmap} fmap g (Just (f x)) = {definition of fmap} Just (g (f x)). This is the same results of the 1st side of the equality. END OF PROOF. 

==========================================================================================================
Part 34: 

Brief Explanation: fmap applies a function, f, to the data wrapped inside of Identity and then wraps Identity around the result to create a value of type :: Identity (f x). 

Mapping any function f to Const y would just result in Const y itself. 

> instance Functor Identity where 
>      fmap f (Identity x) = Identity (f x) 

> instance Functor (Const b) where 
>      fmap f (Const y) = Const y

Proof of two Functor Laws

1. Functional Composition: fmap(g . f) = fmap g . fmap f 
2. Identity :              fmap id     = id 

Proof for Identity Functor:

1. Proof of the 2nd Law: fmap id (Identity x) = id (Identity x) = {applying id} (Identity x)

fmap id (Identity x) = {definition of fmap} Identity (id x) = {applying id} Identity x . END of Proof

2. Proof of the 1st Law: fmap (g . f) (Identity x) = (fmap g . fmap f) Identity x

Starting with 1st part of the equation: fmap (g . f) (Identity x) = {Applying fmap} Identity ((g . f) x) = {function composition} Identity (g (f x))

Now the 2nd part of the euqation: (fmap g . fmap f) Identity x = {function composition} fmap g (fmap f (Identity x)) = {applying fmap} fmap g (Identity (f x)) = {Applying fmap} Identity (g (f x)). 

The same result as first part of the equality. END OF PROOF

/========================================================================================================/
Proof for Const Functor:

1. Proof of the 2nd Law: fmap id (Const x) = id (Const x) = {applying id} Const x

fmap id (Const x) = {definition of fmap for Const} Const x. END OF PROOF 

2. Proof of the 1st Law: fmap (g . f) Const x = (fmap g . fmap f) Const x

starting with left side of equation: fmap (g . f) Const x = {definition of fmap for Const} Const x 

right side of equation: (fmap g . fmap f) Const x = {function application} fmap g (fmap f (Const x)) = {definition of fmap for Const} fmap g (Const x) = {definition of fmap for Const} Const x. END OF PROOF 

==========================================================================================================
Part 35:

Brief Explanation: modifyLensVL takes a LensVL, a modifying function f :: a -> b, applies the modifying function to its source s and returns a potentially modified source t. First of all, LensVL is of type :: Functor f => (a -> f b) -> (s -> f t). As we can see from its siganture, the function return is wrapped with a Functor around it since LensVL requires a function of type (a -> f b) where f is a functor. 

So, we need to change the output of the modifying function to return a functor to match the expected output. As the modifying function is currently returning b type, we wrap Identity Functor around the output of modifying function, f to return a Functor. So, using the Identify functor and composing it with the function f :: a -> b we get : a -> Identity b. We can then apply the lens l to this new focus functor (result of modifying function) and the source s as in LensVL (a -> Identity b) s. This results in Identity t. Finally, applying unIdentity (Identity t) gives us back the potentially updated source t.  

> modifyLensVL :: LensVL a b s t -> (a -> b) -> (s -> t)
> modifyLensVL l f  =  unIdentity . l (Identity . f) 

type checking
f :: a -> b 
modifyLens :: LensVL -> f -> s -> t 
Identity . f :: a -> Identity b 
l (a -> Identity b) s :: Identity t 
unIdentity (Identity t) :: t 

Brief Explanation of updateLensVL: This function takes a LensVL, a source of type s and a focus of type b in curried manner and returns a potentially updated source of type t. It then calls modifyLensVL on the LensVL l provided to the function,  passing, as well, a focus of type b and the source s. It provides (const y) to the function used to modify LensVL. This is done to ensure that the original focus a is ignored in modifying function. Instead, the new focus b (y in this case) is used. This applies needed modifications and returns a potentially updated source of type t

> updateLensVL :: LensVL a b s t -> s -> b -> t
> updateLensVL l s y = modifyLensVL l (const y) s

GHCI Examples:

> lensTuple :: Functor f => (c -> f k') -> (a,b,c) -> f (a,b,k')
> lensTuple f (x,y,z) = fmap (\c -> (x,y, c)) (f z) 

ghci> modifyLensVL lensTuple (^3) (5,6,7)
(5,6,343)

ghci> updateLensVL lensTuple (5,6,7) 100
(5,6,100)
==========================================================================================================
Part 36:

Brief Explanation: viewLensVL takes a LensVL l and a source s to extract a focus a from source s without modifying the source. Const data type can do that as it "hides" its contents within the source, allowing the value to go through operations without being modified. The Lens is applied to the source using Const Functor. By applying unConst we can extract the focus from Const as the focus of Lens lies within Const Functor. 

> viewLensVL :: LensVL a b s t -> s -> a 
> viewLensVL l  = unConst . l Const 

GHCI Examples:

ghci> viewLensVL lensTuple (5,6,7)
7

==========================================================================================================
Part 37.

Brief Explanation: This function takes a Van Laarhoven lens, l', and converts it into PolyLens. The pview function, f'', calls viewLensVL on the Van Laarhoven lens l' which returns the focus. The pupdate function, g'', simply calls updateLensVL on the Van Laarhoven lens l' which returns the updated source t  

> fromLensVL :: LensVL a b s t -> PolyLens a b s t
> fromLensVL l' = PolyLens f'' g'' where 
>    f'' = viewLensVL l' 
>    g'' = updateLensVL l'

GHCI Examples:

ghci> let polyLensTuple = fromLensVL lensTuple
ghci> pview polyLensTuple (5,6,7)
7

ghci> pupdate polyLensTuple (5,6,7) 17
(5,6,17)

ghci> :t polyLensTuple 
polyLensTuple :: PolyLens a1 b1 (a2, b2, a1) (a2, b2, b1)

==========================================================================================================
Part 38. 

Brief Explanation: This function takes a PolyLens and converts it to LensVL. pview function, f, from PolyLens extracts the focus from the source s, which is then used by the modifying function f' to modify it to a focus of type F b (using some functor F). Finally, pupdate function, g, from PolyLens is used to update the contents within the source with the new focus of type (F b), creating a potentially new updated source of type some functor (F t). fmap is used with pupdate (g) to apply transformation F b to source s creating (F t), where F is some Functor. 

> toLensVL :: PolyLens a b s t -> LensVL a b s t 
> toLensVL (PolyLens f g) f' s = fmap (g s) (f' (f s))

GHCi examples:

ghci> let polyLensTuple = fromLensVL lensTuple
ghci> :t polyLensTuple
polyLensTuple :: PolyLens a1 b1 (a2, b2, a1) (a2, b2, b1)

ghci> :t toLensVL (polyLensTuple) 
toLensVL (polyLensTuple) :: LensVL a b (a2, b2, a) (a2, b2, b)

ghci> let conversion_back = toLensVL (polyLensTuple)

ghci> :t conversion_back 
conversion_back
  :: Functor f => (a -> f b) -> (a2, b2, a) -> f (a2, b2, b)

ghci> modifyLensVL conversion_back (^3) (5,6,7)
(5,6,343)

ghci> updateLensVL conversion_back (5,6,7) 100
(5,6,100)

==========================================================================================================
Part 39. 

Brief Explanation: The Part prompt suggests using fstLens but this wouldn't type-check. So, I tried using fstPolyLens. 

fstLensVL maps a function f over a tuple source, focusing only on its first element. The function f is applied to x only, leaving y untouched. 

> fstLensVL :: LensVL a a' (a,b) (a',b)
> fstLensVL f (x,y) = fmap (, y) (f x)

Using toLensVL and fstPolyLens: 

> fstLensVL' :: LensVL a a' (a,b) (a', b)
> fstLensVL' = toLensVL fstPolyLens 

Test to see if these two functions agree: 

> convert :: Int -> Identity Double 
> convert  = Identity . fromIntegral 

GHCI Examples:

ghci> unIdentity $ fstLensVL convert (3, "Anything else")
(3.0,"Anything else")

ghci> unIdentity $ fstLensVL' convert (3, "Anything else")
(3.0,"Anything else")

==========================================================================================================
Part 40

Brief Explanation:

type LensVL a b s t = forall f . Functor f => (a -> f b) -> (s -> f t)

LensVL a' b' a b, l in the function definition, is a function mapping of a function (a' -> f b') to a function (a -> f b), shown below. So, it needs some form of an intermediate function of type :: a' -> f b' to return (a -> f b) which can then be used by Lense l' to return the final output function of type (s -> f t). All shown below:

l  a' b' a b :: (a' -> f b') -> (a -> f b)
l' a b s t   :: (a -> f b) -> (s -> f t) 
f' :: a' -> f b' 
l f' = l (a' -> f b') = (a -> f b) :: a -> f b 
l' (l f') = l' (a -> f b) -> (s -> f t)  :: (s -> f t)

> composeLensVL :: LensVL a' b' a b -> LensVL a b s t -> LensVL a' b' s t 
> composeLensVL l l' f'  = l' ( l f')

GHCI EXAMPLES: (using running examples in this section)

ghci> let polyLensTuple = fromLensVL lensTuple
ghci> :t polyLensTuple 
polyLensTuple :: PolyLens a1 b1 (a2, b2, a1) (a2, b2, b1)
ghci> let lensVL' = toLensVL (polyLensTuple)
ghci> :t lensVL'
lensVL' :: Functor f => (a -> f b) -> (a2, b2, a) -> f (a2, b2, b)
ghci> let composed = composeLensVL lensVL' lensTuple
ghci> :t composed
composed
  :: Functor f =>
     (a' -> f b') -> (a, b, (a2, b2, a')) -> f (a, b, (a2, b2, b'))
ghci> let composed' = fstLensVL composed
ghci> :t composed'
composed'
  :: Functor f =>
     (a' -> f b', b1)
     -> (a, b2, (a2, b3, a')) -> (f (a, b2, (a2, b3, b')), b1)

==========================================================================================================
Part 41 (SEE REFERENCE IN APPENDIX: [4])

Brief Explanation: The definition below is from standard Prelude. I did a bit of reading on Choice and Applicative, where Choice is similar to Either for some Profunctor p. 

type PrismVL a b s t = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

Now I can collapse and simplify this using the traverse clue in the Part, leading to the following definition (as every prism is indeed a valid traversal): 

> type PrismVL a b s t = forall f. Applicative f => (a -> f b) -> s -> f t

Example: 

> leftPrism'' :: PrismVL a b (Either a c) (Either b c)
> leftPrism'' f (Left a) = Left <$> f a
> leftPrism'' f (Right c) = pure (Right c) 

ghci> :t leftPrism''
leftPrism''
  :: Applicative f => (a -> f b) -> Either a c -> f (Either b c)
==========================================================================================================
==========================================================================================================
Part 6. Profunctor Optics:

> type Optic a b s t = forall p . Profunctor p => p a b -> p s t 

> class Profunctor p where 
>    dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b' 

1. dimap (f' . f) (g . g') = dimap f g . dimap f' g' 
2. dimap id id             = id 

Part 42:

Brief Explanation: Here is the sequence of operation (using some diagram like explanations): Given f :: a' -> a ; g :: b -> b' and h :: a -> f b 

INPUT a' -> |FUNCTION f| -> OUTPUT a -> |FUNCTION h| -> OUTPUT f b 

Now we as we have an instance of a Functor, a functor type wrapped around b, we use fmap to map function g over the data type b inside of the functor -> fmap g (f b) -> (f b') as g :: b -> b'

Finally output is:  UpStar (f a' b')

> data UpStar f a b = UpStar (a -> f b)

> instance Functor f => Profunctor (UpStar f) where 
>    dimap f g (UpStar h) = UpStar (fmap g . h . f )

==========================================================================================================
Part 43:

Brief Explanation:  

Here is the sequence of operations: Given : (f' :: a' -> a) ; (g :: b -> b') and (h :: f a -> b )

1st: function f', using fmap, is mapped over the items inside of the input functor (f a'), a functor instance with type a' inside of it to modify the contents inside of the functor from type a' to type a. This would create an output type of (f a), which is then passed to function h. Here is the rest of the operations: 

(f a) -> |FUNCTION h| -> OUTPUT b -> |FUNCTION g| -> b' as g :: b -> b'

Finally output is:  Downstar (f a' b')

> data DownStar f a b = DownStar (f a -> b)

> instance Functor f => Profunctor (DownStar f) where 
>    dimap f' g (DownStar h) = DownStar (g . h . fmap f')

==========================================================================================================
Part 44 

Brief Explanation: I have realized the best way way to explain it would be through this sequence of operations. Given: (f :: a' -> a) and (g :: b -> b') 

INPUT s' -> |FUNCTION f| -> OUTPUT s -> |FUNCTION fr| -> OUTPUT a  
INPUT b -> |FUNCTION to| -> OUTPUT t -> |FUNCTION g| -> OUTPUT t' 

Now by wrapping Adapter around (fr . f) and (g . to) we have a new from' and a new to' functions that now do the following: from' :: some source s' -> a (focus) and to' :: b (focus)-> t' (some target source) 


> data Adapter a b s t = Adapter {from :: s -> a, to :: b -> t}

> instance Profunctor (Adapter fr to) where 
>   dimap f g (Adapter fr to) = Adapter from' to' where 
>       from' s = fr $ f s 
>       to'   y = g $ to y

Alternatively using more concise definition:

instance Profunctor (Adapter fr to) where 
   dimap f g (Adapter fr to) = Adapter (fr . f) (g . to)       
==========================================================================================================
Part 45:

Brief Explanation: Given profunctor functions f and g where f :: a' -> a (a reverve arrow) and the function g :: b -> b' and given an Adapter a b s t with function: fr :: s -> a and to :: b -> t. Looking at type signatures we must ensure that dimap uses the (from) function of Adapter to transform the input (s) in a reverse arrow manner (therefore using fr in place of f) and the (to) function of Adapter to transform output (b) (therefore using to in place of g). This will match Adapter's type signature. Therefore, (dimap fr to) has the correct type p a b -> p s t for some profunctor p.  


> a2o :: Adapter a b s t -> Optic a b s t
> a2o (Adapter fr to)  = dimap fr to 

==========================================================================================================
Part 46: 

Brief Explanation: identity functions (id) from standard Prelude are used from both Adapter's from and to functions. id doesn't make any transformations to the original types. from and to in this case are both id.  

> o2a :: Optic a b s t -> Adapter a b s t
> o2a f  = f (Adapter id id) 

type checking: 

from :: s -> a. This would be s -> s because of id 

to :: b -> t. This would be b -> b because of id 

So, f (s b) where where f is covariant in second argument (mapping b to t) and contravariant in 1st argument (reverse arrow: mapping s to a) returning Adapter a b s t as a result which is a profunctor instance. 
==========================================================================================================
==========================================================================================================
Concluding Remarks: 

I really enjoyed doing this study. Haskell is one of the most natural programming languages for exploring topics in category theory. Thank you! 
==========================================================================================================
==========================================================================================================
References:

The following resources have been consulted during my preparation:
1. J. Gibbons: [Functional Programming Course], University of Oxford 
2. B. Clarke, D. Elkins, J. Gibbons, et al: [Profunctor Optics, A Categorical Update](https://arxiv.org/pdf/2001.07488.pdf)
3. B. Milewski: [Profunctor Polymorphism](https://bartoszmilewski.com/2016/08/16/profunctor-polymorphism/)
4. Haskell Documentation: [Lens-Prism] (https://hackage.haskell.org/package/lens-5.2.3/docs/Control-Lens-Prism.html)
5. R. Bird: [Thinking Functionally with Haskell], Cambridge University Press
6. G. Hutton: [Programming in Haskell, 2nd edition], Cambridge University Press 


