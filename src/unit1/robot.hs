robot (name, attack, hp) = \message ->
    message (name, attack, hp)

killerRobot = robot ("Kill3r",25,200)

name (n, _, _) = n
attack (_, a, _)  = a
hp (_, _, hp) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

setName aRobot newName = aRobot (\(n, a, h) ->
        robot (newName, a, h)
    )
setAttack aRobot newAttack = aRobot (\(n, a, h) ->
        robot (n, newAttack, h)
    )
setHP aRobot newHP = aRobot (\(n, a, h) ->
        robot (n, a, newHP)
    )

printRobot aRobot = aRobot (\(n, a, h) ->
        n
        ++ " attack: " ++ show a
        ++ " hp: " ++ show h
    )

damage aRobot attackDamage = aRobot (\(n, a, h) ->
        robot (n, a, h - attackDamage)
    )

fight aRobot defender = damage defender attack
    where attack = if getHP aRobot > 10
                   then getAttack aRobot
                   else 0

gentleGiant = robot ("Mr. Friendly", 10, 300)

gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 = fight gentleGiant killerRobot
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2


fastRobot = robot ("speedy", 15, 40)
slowRobot = robot ("slowpoke", 20, 30)

slowRobotRound1 = fight fastRobot slowRobot
fastRobotRound1 = fight slowRobotRound1 fastRobot
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
fastRobotRound2 = fight fastRobotRound1 slowRobotRound1
slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
fastRobotRound3 = fight slowRobotRound3 fastRobotRound2


getHPs :: [((a, b1, c) -> c) -> b2] -> [b2]
getHPs aRobots = map getHP aRobots

-- threeRoundFight a b = (\(ab, ba) ->
--             (\(baab, abba) ->
--                 getWinner (fight abba baab) (fight baab abba)
--             ) (fight ba ab, fight ab ba)
--         ) (fight a b, fight b a)
--     where
--         getWinner r1 r2 = case getHP r1 `compare` getHP r2 of
--                             LT -> printRobot r1
--                             GT -> printRobot r2
--                             EQ -> "there are no winner"

three = [gentleGiant, fastRobot, slowRobot]
fightWithKiller = map (getHP . fight killerRobot)
