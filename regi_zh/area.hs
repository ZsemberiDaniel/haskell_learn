type Path = String

mirror :: Path -> Path
mirror [] = []
mirror (step:path) = (mirror path) ++ [(reverseStep step)]
    where reverseStep p
            | p == 'u' = 'd'
            | p == 'd' = 'u'
            | p == 'l' = 'r'
            | p == 'r' = 'l'

isClosed path = isClosed' path (0,0)

isClosed' [] (atX, atY) = (atX == 0) && (atY == 0)
isClosed' (step:path) (atX, atY)
    | step == 'u' = isClosed' path (atX, atY + 1)
    | step == 'd' = isClosed' path (atX, atY - 1)
    | step == 'l' = isClosed' path (atX - 1, atY)
    | step == 'r' = isClosed' path (atX + 1, atY)

points :: Path -> (Int, Int) -> [(Int, Int)]
points path starting = starting:(points' path starting)

points' [] (atX, atY) = []
points' (step:path) (atX, atY)
    | step == 'u' = (atX, atY + 1):(points' path (atX, atY + 1))
    | step == 'd' = (atX, atY - 1):(points' path (atX, atY - 1))
    | step == 'l' = (atX - 1, atY):(points' path (atX - 1, atY))
    | step == 'r' = (atX + 1, atY):(points' path (atX + 1, atY))

area :: Path -> Int
area path = area' pPoints
    where pPoints = points path (0,0)

area' (p0:[]) = snd p0 * (negate $ fst p0)
area' (p0:p1:pathPoints) = snd p0 * (fst p1 - fst p0) + (area' $ p1:pathPoints)

upCount path = sum $ map isUpStep path
    where isUpStep step = if step=='u' then 1 else 0

downCount path = sum $ map isDownStep path
    where isDownStep step = if step=='d' then 1 else 0

leftCount path = sum $ map isLeftStep path
    where isLeftStep step = if step=='l' then 1 else 0

rightCount path = sum $ map isRightStep path
    where isRightStep step = if step=='r' then 1 else 0