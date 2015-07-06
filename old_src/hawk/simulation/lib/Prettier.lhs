\begin{code}
-- New pretty-printing combinators

module Prettier where

infixr 6 :<>

-- the simple concatentation operator
-- chosen not to conflict with Hughes
infixr 6 <:>, <:+>

-- for emulating Hughes
infixr 6 <>
infixr 6 <+>
-- I like this much better than $$
-- and it doesn't give hugs fits (hugs uses $$ as a top-level macro)
-- infixr 5 //
-- fargnarts - array steals this, and hugs doesn't handle
-- scoping of fixity well
infixr 5 $/

data Doc =
      Empty
    | Line
    | Indent Int
    | Text String
    | Nest Int Doc -- not used at present
    | Tab Doc
    | Doc :<> Doc
    | Group Doc
    deriving Show

empty = Empty
line = Line
indent i = Indent i
text s = Text s
tab x = Tab x
x <:> y = x :<> y
group x = Group x

x <:+> y = x :<> text " " :<> y

{- prSimple : layout simple forms
   l - current left margin
   f - should whitespace be flattened?
   q - see below
   k - current (absolute) position

   returns:
   - text to be output
   - new setting of q flag
   - new position

   `line <:> line', or `line <:> indent i' should flatten
   to a single space.  In the following, the flag q (`quiet')
   is used to indicate when additional whitespace should be
   suppressed
-}

prSimple :: (Doc,Int,Bool) -> Bool -> Int -> (String,Bool,Int)
prSimple (Empty, l, f) q k =		("", False, k)

prSimple (Line, l, True) True k =	("", True, k)
prSimple (Line, l, True) False k =	(" ", True, k + 1)
prSimple (Line, l, f) q k =		('\n' : replicate l ' ', False, l)

prSimple (Indent i, l, True) True k =   ("", True, k)
prSimple (Indent i, l, True) False k =	(" ", True, k + 1)
prSimple (Indent i, l, f) q k =
    if k > l + i then
	('\n' : replicate (l + i) ' ', False, l + i)
    else
	(replicate (l + i - k) ' ', False, l + i)

prSimple (Text s, l, f) q k =		(s, False, k + length s)

pr :: Int -> Doc -> (String,Bool,Int)
pr w x = prComb (x, 0, False) [] True 0
    where
    -- layout combining forms
    -- the second arg is an accumulator for flattening out compositions
    -- w is in scope here, so this isn't a top-level def
    prComb :: (Doc,Int,Bool) -> [(Doc,Int,Bool)] ->
	      Bool -> Int -> ([Char],Bool,Int)
    prComb (Nest i x, l, f) ys q k =
	prComb (x, l + i, f) ys q k
    prComb (Tab x, l, f) ys q k =
	prComb (x, k, f) ys q k
    prComb (x :<> y, l, f) zs q k =
	prComb (x, l, f) ((y, l, f) : zs) q k
    prComb (Group x, l, True) ys q k =
	prComb (x, l, True) ys q k
    prComb (Group x, l, False) ys q k =
	let (s, q', k') = prComb (x, l, True) ys q k in
	    if fits s (w - k) w 5 then
		(s, q', k')
	    else
		prComb (x, l, False) ys q k
    prComb x [] q k = prSimple x q k
    prComb x (y : ys) q k =
	let (s, q', k') = prSimple x q k
	    (t, q'', k'') = prComb y ys q' k'
	in
	    (s ++ t, q'', k'')

-- does string xs fit in a line of (relative) width n,
-- with m lines of lookahead?
-- w is the absolute width, used to reset n after a newline
fits xs n w m | n < 0 = False
fits [] n w m = True
fits ('\n':xs) n w 0 = True
fits ('\n':xs) n w m = fits xs w w (m - 1)
fits (x:xs) n w m = fits xs (n - 1) w m

pretty :: Int -> Doc -> String
pretty w x = let (s, _, _) = pr w x in s

-- Hughes

x  <> y = x <:> tab y
x <+> y = x <:> text " " <> y
x  $/ y = x <:> line <:> y
nest i x = indent i <> x

-- Additional handy ops

ppList o c p xs = text o <:> ppsep p (text ", ") xs <:> text c
--ppOptList p [] = empty
--ppOptList p xs = ppList p xs

ppMaybe p Nothing = empty
ppMaybe p (Just x) = p x

ppsep p s = foldr' (\x y -> p x <:> s <> y) p empty
ppfoldr c s n = foldr' c (\x -> s x <:> n) n

foldr' c s n [] = n
foldr' c s n [x] = s x
foldr' c s n (x : xs) = c x (foldr' c s n xs)
\end{code}
