-- ("Bob", 10, 100) = name, attack, HP

-- constructor
robot (name, attack, hp) =
  \message -> message (name, attack, hp)

killer = robot ("Kill3r", 25, 200)

-- accessors
name (n, _, _) = n

attack (_, a, _) = a

hp (_, _, hp) = hp

getName aRobot = aRobot name

getAttack aRobot = aRobot attack

getHP aRobot = aRobot hp

setName aRobot newName = aRobot (\(_, a, h) -> robot (newName, a, h))

setAttack aRobot newAttack = aRobot (\(n, _, h) -> robot (n, newAttack, h))

setHP aRobot newHP = aRobot (\(n, a, _) -> robot (name, a, newHP))

kittyRobot = setName killer "kitty"

printRobot aRobot = aRobot (\(n, a, h) -> n ++ " attack: " ++ show a ++ " hp: " ++ show h)

damage aRobot attackDamage =
  aRobot
    ( \(n, a, h) ->
        robot (n, a, h - attackDamage)
    )

fight aRobot defender = damage defender attack
  where
    attack =
      if getHP aRobot > 10
        then getAttack aRobot
        else 0

gentleGiant = robot ("Mr. Friendly", 10, 300)

gentleGiantRound1 = fight killer gentleGiant

killerRound1 = fight gentleGiant killer

gentleGiantRound2 = fight killerRound1 gentleGiantRound1

killerRound2 = fight gentleGiantRound1 killerRound1

gentleGiantRound3 = fight killerRound2 gentleGiantRound2

killerRound3 = fight gentleGiantRound2 killerRound2
