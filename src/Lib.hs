module Lib where
import Text.Show.Functions

--PUNTO 1

data Raton = Raton {
    nombre :: String,
    edad :: Float,
    peso :: Float,
    enfermedades :: Enfermedades
}deriving (Show, Eq)

type Enfermedades = [String] --por ahora

cerebro :: Raton
cerebro = Raton "Cerebro" 9 0.2 ["brucelosis","sarampion","tuberculosis"]

bicenterrata :: Raton
bicenterrata = Raton "Bicenterrata" 256 0.2 []

huesudo :: Raton
huesudo = Raton "Huesudo" 4 10 ["obesidad","sinusitis"]

--PUNTO 2

type Hierba = Raton -> Raton

hierbaBuena :: Hierba
hierbaBuena  = modificarEdad sqrt 

modificarEdad :: (Float -> Float) -> Raton -> Raton
modificarEdad funcion raton = raton {edad = funcion.edad $ raton}

modificarEnfermedad :: (Enfermedades -> Enfermedades) -> Raton -> Raton
modificarEnfermedad funcion raton = raton {enfermedades = funcion.enfermedades $ raton}

hierbaVerde :: String -> Hierba
hierbaVerde terminacion = modificarEnfermedad (enfermedadesSinCiertaTerminacion terminacion)

enfermedadesSinCiertaTerminacion :: String -> Enfermedades -> Enfermedades
enfermedadesSinCiertaTerminacion terminacion = filter (noTerminaEn terminacion)

noTerminaEn :: String -> String->  Bool
noTerminaEn terminacion enfermedad = (/=) terminacion (take (length terminacion) (reverse enfermedad))

alcachofa :: Hierba
alcachofa = modificarPeso perderPeso

modificarPeso :: (Float -> Float) -> Raton -> Raton
modificarPeso funcion raton = raton {peso = funcion.peso $ raton}

perderPeso :: Float -> Float
perderPeso peso
    | (>2) peso = restarPeso 10 peso
    | otherwise = restarPeso 5 peso

restarPeso :: Float -> Float -> Float
restarPeso porcentaje peso = peso - ((*)(porcentaje/100) peso)

hierbaZort :: Hierba
hierbaZort  = modificarNombreAPinky.eliminarTodasLasEnfermedades.modificarEdad (\edad->0)


modificarNombreAPinky :: Raton -> Raton
modificarNombreAPinky raton = raton {nombre = "Pinky"}

eliminarTodasLasEnfermedades :: Raton -> Raton
eliminarTodasLasEnfermedades raton = raton {enfermedades = []}

hierbaDelDiablo :: Hierba
hierbaDelDiablo = (modificarPeso perderPesoDiablo).(modificarEnfermedad enfermedadesConMenosDe10Letras)

perderPesoDiablo :: Float -> Float
perderPesoDiablo peso = max 0 (peso-0.1)

enfermedadesConMenosDe10Letras :: Enfermedades -> Enfermedades
enfermedadesConMenosDe10Letras enfermedades = filter ((<10).length) enfermedades

--PUNTO 3

type Medicamento = Raton -> Raton

pondsAntiAge :: Medicamento
pondsAntiAge = hierbaBuena.hierbaBuena.hierbaBuena.alcachofa

reduceFatFast :: String -> Int -> Medicamento
reduceFatFast terminacion potencia raton = (potenciaAlcachofa potencia).(hierbaVerde terminacion) $ raton 
--tengo un problema con hirbaVerde (funciona cuando solo le paso un string de 3 caracteres)

--EJEMPLOS DE CONSOLA
{-
*Lib Lib> hierbaVerde "dad" huesudo
Raton {nombre = "Huesudo", edad = 4.0, peso = 10.0, enfermedades = ["sinusitis"]}

*Lib Lib> hierbaVerde "idad" huesudo
Raton {nombre = "Huesudo", edad = 4.0, peso = 10.0, enfermedades = ["obesidad","sinusitis"]}

 potenciaAlcachofa 2 huesudo
Raton {nombre = "Huesudo", edad = 4.0, peso = 8.1, enfermedades = ["obesidad","sinusitis"]}
ME TENDRIA QUE DEVOLVER SOLO SINUSITIS (potenciaAlcachofa anda bien)

-}
potenciaAlcachofa :: Int -> (Medicamento)
potenciaAlcachofa 1 = alcachofa
potenciaAlcachofa potencia = alcachofa.(potenciaAlcachofa (potencia-1))

sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

pdepCilina :: Medicamento
pdepCilina raton = foldr hierbaVerde raton sufijosInfecciosas


