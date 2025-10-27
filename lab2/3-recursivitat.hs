-- Exercici: Fent servir recursivitat calcular la divisió entera i la resta:
divisio :: Int -> Int -> Int
divisio _ 0 = 0                -- decisió: si divisor és 0, retornem 0
divisio n d
  | n < d     = 0
  | otherwise = 1 + divisio (n - d) d -- n = d + (n − d), quants cops cap d dins (n − d)

residu :: Int -> Int -> Int
residu _ 0 = 0                  -- decisió: residu 0 si divisor 0
residu n d
  | n < d     = n
  | otherwise = residu (n - d) d

-- Exercici (Coeficienet binomial):
binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k = binomial (n - 1) (k - 1) + binomial (n - 1) k
