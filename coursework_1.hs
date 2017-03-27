
import Data.Char

------------------------- The map

type Location = String
type Map      = [(Location,Location)]

accessible :: Location -> [Location]
accessible l = [ b | (a,b) <- theMap , a == l ] ++
               [ a | (a,b) <- theMap , b == l ]

------------------------- World contents

type Object   = String
type Contents = [(Location,[Object])]

findContents :: Location -> Contents -> [Object]
findContents _ [] = []
findContents l ((k,xs):con)
    | l == k    = xs
    | otherwise = findContents l con

addContents :: Location -> [Object] -> Contents -> Contents
addContents _ _ [] = []
addContents l ys ((k,xs):con)
    | k == l    = (k, xs ++ ys) : con
    | otherwise = (k,xs) : addContents l ys con

-------------------------

removeOne :: Eq a => [a] -> a -> [a]
removeOne    []  _ = []
removeOne (x:xs) y
    | x == y    = xs
    | otherwise = x : removeOne xs y

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll xs    []  = xs
removeAll xs (y:ys) = removeAll (removeOne xs y) ys

removeContents :: Location -> [Object] -> Contents -> Contents
removeContents _ _ [] = []
removeContents l ys ((k,xs):rest)
    | k == l    = (k, removeAll xs ys) : rest
    | otherwise = (k,xs) : removeContents l ys rest


-- isInList :: [Object] -> [Object] -> Bool
-- isInList _ [] = False
-- isInList

-------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort (take n xs)) (msort (drop n xs))
  where
    n = div (length xs) 2

equal :: [Object] -> [Object] -> Bool
equal xs ys = msort xs == msort ys


------------------------- Game

type Game  = (Location,[Object], Contents)

--empty :: Game
empty = ("",[],[])


------------------------- PART 1: Events
type Event = Game->Game

receive :: [Object] -> Event
receive ys (l,xs,c) = (l, ys ++ xs,c)


deposit :: [Object] -> Event
deposit ys (l, xs, c) = (l, xs, (addContents l ys c ))

remove :: [Object] -> Event
remove ys (l, xs, c) = (l, (removeAll ys xs), (removeContents l ys c))

leave :: [Object] -> Event
leave ys (l, xs, c) = (l, (removeAll xs ys), (addContents l ys c ))
   -- | ys `elem` [xs] = (l, (removeAll xs ys), (addContents l ys c ))
   -- | otherwise    = (l, xs, c)

bring :: [Object] -> Event
bring ys (l, xs, c) = (l, xs ++ ys, (removeContents l ys c))
   -- | ys `elem` [(findContents l c)] = (l, xs ++ ys, (removeContents l ys c))
   -- | otherwise = (l, xs, c)

removeFromLocation :: [Object] -> Location -> Event
removeFromLocation ys lg (l,xs,c) = (l, xs, (removeContents lg ys c ))

depositAtLocation :: [Object] -> Location -> Event
depositAtLocation ys lg (l,xs,c) = (l, xs, (addContents lg ys c ))


------------------------- PART 2: Dialogue

data Dialogue = End     String  Event
              | Choice  String  [( String , Dialogue )]

dialogue :: Game -> Dialogue -> IO Game
dialogue g (End s e) = do
  putStr s
  return (e g)
dialogue (l, xs, c) (Choice s ys) = do
  putStrLn s
  putStr (enumerate 1 (map fst ys))
  input <- getLine
  if input `elem` exitWords
    then do
      return (l, xs, c)
    else do
      let k = snd (ys !! (read input -1))
      dialogue (l, xs, c) k



 -- dialogue (l, xs, c) (End s e) = do
--   putStr s
--   return e 



bridgeScene :: Dialogue
bridgeScene = Choice s1
  [ ( s2 , Choice q1
    [ ( a1 , Choice q2
      [ ( a2 , Choice q3
        [ ( a3 , End e1 id) ] )
      ] )
    , ( a4 , Choice q2
      [ ( a2 , Choice q4
        [ ( a5 , End e2 id) ] )
      ] )
    , ( a6 , Choice q2
      [ ( a2 , Choice q3
        [ ( a9 , End e2 id) ] )
      ] )
    , ( a7 , Choice q2
      [ ( a2 , Choice q5
        [ ( a8 , End e3 id) ] )
      ] )
    ] )
  ]
   where s1  = "Stop. Who would cross the Bridge of Death must answer me these questions three, ere the other side he see."
         s2 = "Ask me the questions, bridgekeeper. I am not afraid."
         q1 = "What... is your name?"
         q2 = "What... is your quest?"
         q3 = "What... is your favourite colour?"
         q4 = "What... is the capital of Assyria?"
         q5 = "What... is the air-speed velocity of an unladen swallow?"

         a1 = "My name is Sir Lancelot of Camelot."
         a2 = "To seek the Holy Grail."
         a3 = "Blue."
         a4 = "Sir Robin of Camelot."
         a5 = "I don't know that!"
         a6 = "Sir Galahad of Camelot."
         a7 = "It is Arthur, King of the Britons."
         a8 = "What do you mean? An African or a European swallow?"
         a9 = "Blue. No-"

         e1 = "Right. Off you go."
         e2 = "[Thunk] WAAAAaaaaaauuuuggh"
         e3 = "Huh? I... I don't know that! [Thunk] WAAAAaaaaaauuuuggh"



------------------------- PART 3: Actions

type Action = ([Object],Dialogue)

findDialogue :: [Object] -> Dialogue
findDialogue o 
   | length (filter ((equal o).fst) actions) > 0 = snd(head(filter ((equal o).fst) actions))
   | otherwise = (End "There is nothing we can do." id)


numbers :: String -> [Int]
numbers s = map read (words s)
   -- | length (filter (isDigit.head) (words s)) == length (words s) =  map read (words s)
   -- | otherwise = error "Invalid input" --TO DO: MAKE IT THROW AN ERROR 

findNs ::  Int -> [Int]  -> [Object] -> [Object] -> [Object] 
findNs _ [] _ _ = []  
findNs i (n:ns) o c 
    | n > i && n <= (i+(length o)) = (o !! (n-1-i)) : findNs i ns o c
    | n > (i+(length o)) && n <= ((i+(length o)) + (length c)) = (c !! (n-1-i-(length o))) : findNs i ns o c
    | otherwise = error "Number not scope"

------------------------- PART 4: Game loop
loop :: Game -> IO Game
loop (l,ys,c) = do
  let xs = accessible l 
  putStrLn ("You are in " ++ l) 
  putStrLn ("You can travel to")
  putStr (enumerate 1 xs) 
  putStrLn ("With you are")
  putStr (enumerate (length xs + 1) ys)
  putStrLn ("You can see")
  putStr (enumerate (length xs + length ys + 1) (findContents l c))
  str <- getLine 
  if str `elem` exitWords 
    then do 
      return (l,ys, c)
    else do
      let ns = numbers str
      if length ns == 1 && head ns <= length xs
        then do 
          let k = (xs !! (head(ns)-1))
          loop (k, ys, c)
        else do
          let o = findNs (length xs) ns ys (findContents l c)
          let d = findDialogue o
          g <- (dialogue (l,ys,c) d) 
          loop g



      -- let k = (xs !! (read str -1))

 




game :: IO ()
game = do
  loop start
  return ()


------------------------- Auxiliary functions

exitWords = ["X","x","Q","q","Exit","exit","Quit","quit"]

enumerate :: Int -> [String] -> String
enumerate n xs = unlines [ "  " ++ show i ++ ". " ++ x | (i,x) <- zip [n..] xs ]


------------------------- Spoiler-free game data


--start :: Game
start = ("Nintendo Land",[],people)

people :: Contents
people = [ ("Macon"            , ["Lee"] )
         , ("Pallet Town"      , ["Team Rocket"] )
         , ("Princess Castle"  , ["Rochelle","Peach"] )
         , ("Aperture Science" , ["Portal Gun"])
         , ("Church of Halo"   , ["Priest"])
         , ("Nintendo Land"    , ["Chell","Cortana","Mario","Master Chief"])
         ]

theMap :: Map
theMap =
  let
    b = "Macon"
    c = "Pallet Town"
    k = "Princess Castle"
    l = "Aperture Science"
    t = "Church of Halo"
    w = "Nintendo Land"
  in [(c,l), (c,w), (w,t), (t,k), (k,b)]




actions :: [Action]
actions =
 [ (["Mario"] , Choice "I need to save the Princess."
     [("Sure." , End "Let's go." (bring ["Mario"]))
     ,("Not right now." , End "Ok." (leave ["Mario"]))
     ])
 , (["Mario","Peach"] , Choice "Save me, Mario!"
    [("Sure." , End "Thank you for bringing me my hero. Now I can conveniently leave this hat behind." (deposit ["Baseball Cap"] . remove ["Mario","Peach"]))
    ,("Not right now." , End "Mario, pls." id)])
 , (["Master Chief"] , Choice "I want to marry Cortana. Can you escort us to the Church of Halo?"
     [("Sure." , End "Let's go." (bring ["Master Chief"]))
     ,("Not right now." , End "Ok." (leave ["Master Chief"]))
     ])
 , (["Cortana"] , Choice "I must go with Master Chief."
     [("Sure." , End "Let's go." (bring ["Cortana"]))
     ,("Not right now." , End "Ok." (leave ["Cortana"]))
     ])
 , (["Master Chief","Priest"] , End "I can't marry you without your bride-to-be." id)
 , (["Cortana","Priest"] , End "I can't marry you without your husband-to-be." id)
 , (["Cortana","Master Chief","Priest"] , End "What a beautiful wedding said the bridesmaid to the waiter. But what a shame, that there's some child lurking nearby." (remove ["Cortana","Master Chief","Priest"] . deposit ["Clementine (hiding)"]) )
 , (["Baseball Cap"] , Choice "It's a bit grubby, shall I take it?"
     [("Sure." , End "Let's go." (bring ["Baseball Cap"]))
     ,("Not right now." , End "Ok." (leave ["Baseball Cap"]))
     ])
 , (["Clementine (hiding)"] , End "I'm scared. Where are my parents?" id)
 , (["Baseball Cap", "Clementine (hiding)"] , Choice "Give the girl the hat?"
    [("Sure." , End "I feel safe." (receive ["Clementine"] . remove ["Clementine (hiding)","Baseball Cap"]))
    ,("Not right now." , End "" id)
    ])
 , (["Clementine"] , Choice "Will you help me find my parents?"
     [("This way." , End "Thanks!" (bring ["Clementine"]))
     ,("Not today." , End "Oh, okay." (leave ["Clementine"]))
     ])
  , (["Clementine","Lee"] , Choice "GIVE ME BACK CLEMENTINE!"
     [("Sure...", End "" (remove ["Lee","Clementine"] . receive ["Zombie Lee"]))
     ])
  , (["Lee"] , End "Clem? Clem, where are you?!" id)
  , (["Zombie Lee"] , Choice "Uuurrurrhgghghhghgg."
     [("This way."  , End "" (bring ["Zombie Lee"]))
     ,("Not today." , End "" (leave ["Zombie Lee"]))
     ])
  , (["Rochelle"] , End "Girl, you should pray there aren't no Zombies around." id)
  , (["Rochelle", "Zombie Lee"] , End "What?! A zombie? You've left me for dead!" (deposit ["Pikachu"] . remove ["Rochelle","Zombie Lee"]))
  , (["Chell"] , Choice "I've just got a volunteering position at Aperture Science. Can you help me find it? I'm not good with directions."
     [("This way."  , End "" (bring ["Chell"]))
     ,("Not today." , End "" (leave ["Chell"]))
    ])
  , (["Chell","Portal Gun"] , End "This is your fault. It didn't have to be like this. I'm not kidding, now! Turn back, or I will kill you! I'm going to kill you, and all the cake is gone! You don't even care, do you? This is your last chance! ." (remove ["Chell","Portal Gun"] . depositAtLocation ["Ash"] "Pallet Town" . removeFromLocation ["Team Rocket"] "Pallet Town" ))
  , (["Team Rocket"] , End "Oh, prepare for trouble, that's what they should do. And make it double, we're grabbing Pikachu." id) 
  , (["Pikachu"] , Choice "Pika-Pika"
     [("*throw pokeball*"  , End "" (bring ["Pikachu"]))
     ,("Nope." , End "" (leave ["Pikachu"]))
     ])
  , (["Ash", "Pikachu"] , End "You win." (\_ -> ("Pallet Town",[],[])))
  , (["Pikachu","Team Rocket"] , End "You lose. Bring Pikachu to his rightful owner." (\_ -> ("Pallet Town",[],[])))
 ]







