data Event = Done | In Int | Out Int deriving (Show, Eq)
data State = S0 | S1 Int deriving (Show, Eq)
data TransNode = TransNode {state :: State, trans :: [(Event, State)]} deriving (Show, Eq)
--data lts = lts 

range = [0..2]

thread_P = \s -> case s of
    S0        -> [(\x -> (In x, S1 x)) x | x <- range]
    S1 x      -> [(Out x, S0)]

--trans_list [] = []
--trans_list (x:xs) = (snd x, fst x):trans_list(xs)

gen_scanlist s v q = if ((lookup s v) == Nothing && (elem s q) == False)
                then q ++ [s]
                else q

gen_scanlist_iter [] v q = q
gen_scanlist_iter (x:xs) v q = gen_scanlist x v (gen_scanlist_iter xs v q)

gen_state_vec node v = if ((lookup (state node) v) == Nothing)
        then v ++ [((state node), (length v, node))]
        else []
gen_trans s thread = TransNode {state = s, trans = (thread s)}

t2statelist [] = []
t2statelist (x:xs) = (snd x):(t2statelist xs)
n2tstate node = t2statelist (trans node)

scan_iter [] v thread = v
scan_iter s v thread = scan_iter scan vec thread
            where
                state   = head(s)
                node    = gen_trans state thread
                vec     = gen_state_vec node v
                scan    = gen_scanlist_iter (n2tstate node) vec (tail s)

unfold s thread = scan_iter [s] [] thread

print_trans [] = do putStr $ ""
print_trans (x:xs) = do
    print $ x
    print_trans xs

print_state [] = do putStr $ ""
print_state (x:xs) = do
    putStrLn $ show (fst x) ++ " ID: " ++ show (fst (snd x))
    print_trans (trans (snd (snd x)))
    print_state xs
--    print_trans (snd (snd x))

main :: IO()
main = do
    let p = unfold S0 thread_P
    print_state p
