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
--tengo un problema con hierbaVerde (funciona cuando solo le paso un string de 3 caracteres)

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

--PUNTO 4

cantidadIdeal :: (Int -> Bool) -> Int
cantidadIdeal condicion = primeroEnCumplir condicion

primeroEnCumplir :: (Int -> Bool) -> Int
primeroEnCumplir condicion = head (filter condicion [1..])

tieneSobrepeso :: Raton -> Bool
tieneSobrepeso  = (>1).peso

noTieneSobrepeso :: Raton -> Bool
noTieneSobrepeso = not.tieneSobrepeso

lograrEstabilizar :: [Raton] -> Medicamento ->  Bool
lograrEstabilizar comunidadRatones medicamento = (&&) (ningunoTienenSobrepeso comunidadRatones medicamento) (todosTienenMenosDe3Enfermedades comunidadRatones medicamento)

aplicarMedicamento :: [Raton] -> Medicamento -> [Raton]
aplicarMedicamento ratones medicamento = map medicamento ratones

ningunoTienenSobrepeso :: [Raton] -> Medicamento -> Bool
ningunoTienenSobrepeso ratones medicamento = all (noTieneSobrepeso) (aplicarMedicamento ratones medicamento)

tienenMenosDe3Enfermedades :: Enfermedades -> Bool
tienenMenosDe3Enfermedades = (<3).length

todosTienenMenosDe3Enfermedades :: [Raton] -> Medicamento -> Bool
todosTienenMenosDe3Enfermedades ratones medicamento = all (tienenMenosDe3Enfermedades) (map enfermedades (aplicarMedicamento ratones medicamento))

potenciasIdeales :: [Raton] -> String -> Int
potenciasIdeales ratones terminacion
 |lograrEstabilizar ratones (alcachofa.(hierbaVerde terminacion)) = 1
 |otherwise= 1 + potenciasIdeales ratones terminacion
--porque no usar reduceFatFast (no entendi bien)

--PUNTO 5
{-Queremos saber si un medicamento logra estabilizar una comunidad infinita . ¿Podemos saberlo?
Responder en estos dos casos:
a. Si todos los ratones quedan con menos de 1kg y
sin enfermedades. Justificar.
b. Si un ratón queda con 2kg  y 4 enfermedades.

a) No se sabrá si todos los ratones quedan con menos de 1 kg y sin enfermedades, porque al
tener una lista infinita y un metodo de evaluacion "all" Haskell no nos puede mostrar un 
resultado hasta que no termine de evaluar (al ser infinita no termina nunca de evaluar)

b)En el caso que un raton quede con 2kg y 4 enfermedades, esta vez si, nos va a devolver un False. 
Siempre que encuentre algun elemento en la lista que lo cumpla. Si no cumple ninguno
seguirá evaluando infinitamente por el "all" previamente mencionado. 

-}





