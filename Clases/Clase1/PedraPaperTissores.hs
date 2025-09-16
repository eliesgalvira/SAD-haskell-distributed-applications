data JugadaPPT = Pdr | Ppr | Tsr
    deriving (Show)
data Resultat = Empat | Guanya JugadaPPT
    deriving (Show)

jugar :: JugadaPPT -> JugadaPPT -> Resultat
jugar Pdr Ppr = Guanya Ppr
jugar Tsr Ppr = Guanya Tsr
jugar Ppr Ppr = Empat
jugar Pdr Tsr = Guanya Pdr
jugar Tsr Tsr = Empat
jugar Ppr Tsr = Guanya Tsr
jugar Pdr Pdr = Empat
jugar Tsr Pdr = Guanya Pdr
jugar Ppr Pdr = Guanya Ppr
