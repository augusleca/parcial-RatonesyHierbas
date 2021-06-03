module Library where
import PdePreludat

data Raton = UnRaton {
    nombre :: String
,   edad :: Number
,   peso :: Number
,   enfermedades :: [String]
}deriving (Show,Eq)

-- 1) Modelar Ratones

cerebro = UnRaton {
    nombre = "Cerebro"
,   edad = 9 
,   peso = 0.2
,   enfermedades = ["brucelosis","sarampion","tuberculosis"]
}

bicenterrata = UnRaton {
    nombre = "Bicenterrata"
,   edad = 256 
,   peso = 0.2
,   enfermedades = []
}

huesudo = UnRaton {
    nombre = "Huesudo"
,   edad = 4 
,   peso = 10
,   enfermedades = ["alta obesidad", "sinusitis"]
}

-- 2) Tipos de hierbas

type Hierba = Raton -> Raton

aplicarHierbaRaton :: Raton -> Hierba -> Raton
aplicarHierbaRaton raton hierba = hierba raton 

-- a)
hierbaBuena :: Hierba
hierbaBuena raton = raton {edad = sqrt (edad raton)}

-- b)
hierbaVerde :: String -> Hierba
hierbaVerde letrasEnfermedad raton = raton {enfermedades = filtrarEnfermedadTerminaEn letrasEnfermedad raton}

filtrarEnfermedadTerminaEn :: String -> Raton -> [String]
filtrarEnfermedadTerminaEn letrasEnfermedad raton = filter (enfermedadTerminaEn letrasEnfermedad) (enfermedades raton)

enfermedadTerminaEn :: String -> String -> Bool
enfermedadTerminaEn letrasEnfermedad = not.(terminacionesEnfermedades letrasEnfermedad)

terminacionesEnfermedades :: String -> String -> Bool
terminacionesEnfermedades letrasEnfermedad enfermedad = letrasEnfermedad == (reverse (take (length letrasEnfermedad) (reverse enfermedad)))

-- c)
alcachofa :: Hierba
alcachofa raton
    | (peso raton) > 2 = raton {peso = (peso raton) * 0.9}
    | otherwise = raton {peso = (peso raton) * 0.95}

-- d)
hierbaZort :: Hierba
hierbaZort raton = raton {edad = 0, enfermedades = []}

-- e)
hierbaDelDiablo :: Hierba
hierbaDelDiablo = (eliminarEnfermedadesMenoresA 10).perderPeso

perderPeso :: Raton -> Raton
perderPeso raton = raton {peso = max 0 (peso raton - 0.1)}

eliminarEnfermedadesMenoresA :: Number -> Raton -> Raton
eliminarEnfermedadesMenoresA numero raton = raton {enfermedades = filter ((>numero).length) (enfermedades raton)}

-- 3) Medicamentos

type Medicamento = [Hierba]

aplicarMedicamentoRaton :: Medicamento -> Raton -> Raton
aplicarMedicamentoRaton medicamento raton = foldl aplicarHierbaRaton raton medicamento

--a)
pondsAntiAge :: Medicamento
pondsAntiAge = [hierbaBuena,hierbaBuena,hierbaBuena,alcachofa]

--b)
reduceFatFast :: Number -> Medicamento
reduceFatFast potencia = [hierbaVerde "obesidad"] ++ cuantasAlcachofasSegunPotencia potencia

cuantasAlcachofasSegunPotencia :: Number -> [Hierba]
cuantasAlcachofasSegunPotencia potencia = take potencia (repeat alcachofa)

--c)
sufijosInfecciosas = ["sis","itis","emia","cocos"]

pdepCilina :: Medicamento
pdepCilina = map hierbaVerde sufijosInfecciosas

-- 4) Experimentos

-- a)
cantidadIdeal :: (Number -> Bool) -> Number
cantidadIdeal condicion = cualNaturalCumplePrimero condicion naturalesInfinitos

cualNaturalCumplePrimero :: (Number -> Bool) -> [Number] -> Number
cualNaturalCumplePrimero condicion listanumeros
    | cumpleCondicion condicion (head listanumeros) = head listanumeros
    | otherwise = cualNaturalCumplePrimero condicion (drop 1 listanumeros)

cumpleCondicion :: (Number -> Bool) -> Number -> Bool
cumpleCondicion condicion numero = condicion numero

naturalesInfinitos = iterate (+1) 1

-- b)
lograEstabilizar :: Medicamento -> [Raton] -> Bool
lograEstabilizar medicamento ratones = all condicionCumplirEstabilizar (aplicarMedicamentoRatones medicamento ratones)

aplicarMedicamentoRatones :: Medicamento -> [Raton] -> [Raton]
aplicarMedicamentoRatones medicamento ratones = map (aplicarMedicamentoRaton medicamento) ratones

condicionCumplirEstabilizar :: Raton -> Bool
condicionCumplirEstabilizar raton = (tieneSobrePeso raton == False) && (tieneMenosDe3Enfermedades raton)

tieneSobrePeso :: Raton -> Bool
tieneSobrePeso raton = peso raton > 1

tieneMenosDe3Enfermedades :: Raton -> Bool
tieneMenosDe3Enfermedades raton = length (enfermedades raton) < 3

-- c)
encontrarPotenciaIdealFatFast :: [Raton] -> Number
encontrarPotenciaIdealFatFast ratones = cuantaPotenciaNecesaria 1 ratones

cuantaPotenciaNecesaria :: Number -> [Raton] -> Number
cuantaPotenciaNecesaria potencia ratones 
    | lograEstabilizar (reduceFatFast potencia) ratones = potencia
    | otherwise = cuantaPotenciaNecesaria (potencia+1) ratones

-- 5)

{- En estos casos, solo la opcion b) es la posible,
 ya que al referirse a la a) nos pide que TODOS (all) los ratones cumplan 
 una cualidad, lo cual es IMPOSIBLE ya que al ser infinitos ratones
 no podremos corroborar para todos. En el caso b) es que haya
 ALGUN (any) raton que cumpla la cualidad deseada, en ese caso Haskell, al
 ser un programa lazy, se fijara elemento por elemento y cuando encuentre uno
 lo devolvera, sin importar que sea infinita la lista de ratones -}

-- 6) 
