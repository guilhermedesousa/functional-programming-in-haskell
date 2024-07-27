-- dados dois pesos w1, w2, retorna uma função que
-- calcula a média ponderada de p1, p2
mediaPonderada :: (Eq a, Floating a) => a -> a -> (a -> a -> a)
mediaPonderada w1 w2
    | somaPesos == 1.0 = \p1 p2 -> (p1 * w1 + p2 * w2) / somaPesos
    | otherwise        = error "Os pesos devem somar 1.0"
    where
        somaPesos = w1 + w2

-- converte uma nota final em conceito
converteNota :: (Ord a, Floating a) => a -> Char
converteNota nota
    | nota < 5 = 'F'
    | nota < 6 = 'D'
    | nota < 7 = 'C'
    | nota < 8 = 'B'
    | otherwise = 'A'

-- calcula conceito final
conceitoFinal :: Char -> Char -> Char
conceitoFinal 'A' 'A' = 'A'
conceitoFinal 'A' 'B' = 'A'
conceitoFinal 'A' 'C' = 'B'
conceitoFinal 'A' 'D' = 'B'
conceitoFinal 'A' 'F' = 'F'
conceitoFinal 'B' 'A' = 'B'
conceitoFinal 'B' 'B' = 'B'
conceitoFinal 'B' 'C' = 'B'
conceitoFinal 'B' 'D' = 'C'
conceitoFinal 'B' 'F' = 'F'
conceitoFinal 'C' 'A' = 'B'
conceitoFinal 'C' 'B' = 'C'
conceitoFinal 'C' 'C' = 'C'
conceitoFinal 'C' 'D' = 'C'
conceitoFinal 'C' 'F' = 'F'
conceitoFinal 'D' 'A' = 'C'
conceitoFinal 'D' 'B' = 'C'
conceitoFinal 'D' 'C' = 'D'
conceitoFinal 'D' 'D' = 'D'
conceitoFinal 'D' 'F' = 'F'
conceitoFinal 'F' _   = 'F'

turmaA1Pratica = mediaPonderada 0.4 0.6
turmaA1Teoria  = mediaPonderada 0.3 0.7

p1A1P = 3
p2A1P = 8
p1A1T = 7
p2A1T = 10

mediaP = turmaA1Pratica p1A1P p2A1P
mediaT = turmaA1Pratica p1A1T p2A1T

finalA1 = conceitoFinal (converteNota mediaP) (converteNota mediaT)

turmaA2Pratica = mediaPonderada 0.4 0.6
turmaA2Teoria = mediaPonderada 0.3 0.9

p1A2P = 3
p2A2P = 8
p1A2T = 7
p2A2T = 10

mediaA2P = turmaA2Pratica p1A2P p2A2P
mediaA2T = turmaA2Teoria p1A2T p2A2T

finalA2 = conceitoFinal (converteNota mediaA2P) (converteNota mediaA2T)