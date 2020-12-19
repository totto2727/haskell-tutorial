robot :: ([Char], Int, Int) -> (([Char], Int, Int) -> t) -> t
robot (name, attack, hp) message = message (name, attack, hp)

helperName :: (a, b, c) -> a
helperName (n, _, _) = n

helperAttack :: (a, b, c) -> b
helperAttack (_, a, _) = a

helperHp :: (a, b, c) -> c
helperHp (_, _, h) = h

getName :: (((a, b, c) -> a) -> t) -> t
getName aRobot = aRobot helperName

getAttack :: (((a, b, c) -> b) -> t) -> t
getAttack aRobot = aRobot helperAttack

getHp :: (((a, b, c) -> c) -> t) -> t
getHp aRobot = aRobot helperHp

setName :: (((a, Int, Int) -> (([Char], Int, Int) -> t1) -> t1) -> t2) -> [Char] -> t2
setName aRobot newName = aRobot (\(_, a, h) -> robot (newName, a, h))

setAttack :: ((([Char], b, Int) -> (([Char], Int, Int) -> t1) -> t1) -> t2) -> Int -> t2
setAttack aRobot newAttack = aRobot (\(n, _, h) -> robot (n, newAttack, h))

setHp :: ((([Char], Int, c) -> (([Char], Int, Int) -> t1) -> t1) -> t2) -> Int -> t2
setHp aRobot newHp = aRobot (\(n, a, _) -> robot (n, a, newHp))

printRobot :: (Show a1, Show a2) => ((([Char], a1, a2) -> [Char]) -> t) -> t
printRobot aRobot =
  aRobot
    ( \(n, a, h) ->
        "Name:"
          ++ n
          ++ "\nAttack:"
          ++ show a
          ++ "\nHP:"
          ++ show h
    )

damage :: ((([Char], Int, Int) -> (([Char], Int, Int) -> t1) -> t1) -> t2) -> Int -> t2
damage aRobot attackDamage=aRobot (\(n,a,h)->robot (n,a,h-attackDamage))

fight :: (((a, b, c) -> b) -> Int) -> ((([Char], Int, Int) -> (([Char], Int, Int) -> t1) -> t1) -> t2) -> t2
fight attacker defender=damage defender attack
  where attack=getAttack attacker

fight2 :: ((([Char], Int, Int) -> (([Char], Int, Int) -> t1) -> t1) -> t2) -> (((a, b, c) -> b) -> Int) -> t2
fight2 defender attacker=damage defender attack
  where attack=getAttack attacker
  
printRobotList :: (Show a1, Show a2) => [(([Char], a1, a2) -> [Char]) -> b] -> [b]
printRobotList = map printRobot


