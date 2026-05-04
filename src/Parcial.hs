PdePerritos

-- Perro
type Perro = (Nombre, Raza, JuguetesFavoritos, TiempoPermanencia, Energia)

type Nombre = String
type Raza = Int

type Juguete = String
type Juguetes = [ Juguete ]
--juguetes :: Juguetes
--juguetes = [ "Pelota", "Mantita", "Peine de Goma"]
type JuguetesFavoritos :: Juguetes

type TiempoPermanencia = String
type Energia = Int

-- Guarderia
type Guarderia = (Perro, Rutina )




type Actividad = ( Ejercicios, TiempoDuracionMin)
type Ejercicio =  Perro -> Perro 00000
type Ejercicios = [ Ejercicio ]
type TiempoDuracionMin = Int

type Actividades = [ Actividad ]
type Rutina = Actividades






-- Rutinas

rutinaJugar :: Actividades 
rutinaJugar = (jugar, 30)

rutinaLadrar18 :: Actividades 
rutinaLadrar18 = ( ladrar, 20 )

regalarPelota :: Actividades 
regalarPelota = (regalar, 0)

diaDeSpa :: Actividades 
diaDeSpa = (diaDeSpaEjercicio, 120)

diaDeCampo :: Actividades 
diaDeCampo = (diaDeCampoEjercicio, 720)





perroZara :: Perro
perroZara = ("Zara", "Dalmata", ("Pelota", "Mantita"), "1 hora y media", 80 )

 

  -- Ejercicios
    -jugar
jugar :: Ejercicio
jugar unPerro = (restarEnergia 10).(energiaMayor0 == true) $ unPerro

energiaMayor0 :: Perro -> Bool
energiaMayor0 unPerro = elem (>0) energia unPerro 

energia :: Perro -> Energia
energia (_, _, _, unaEnergia) = unaEnergia

restarEnergia :: Int -> Perro -> Perro
restarEnergia unNumero unPerro = unPerro - unNumero

    -ladrar
ladrar :: Ejercicio ___ energia
ladrar unPerro = aumentarLadridos 1.5 unPerro

aumentarLadridos :: Int -> Perro -> Perro
aumentarLadridos unaCantidad unPerro = unPerro * unaCantidad

    -regalar
regalar :: Juguete -> Ejercicio
regalar unJuguete unPerro = aniadirJugueteFavorito unPerro

aniadirJugueteFavorito :: Juguete -> Perro -> Perro
aniadirJugueteFavorito unJuguete unPerro = (unJuguete:).juguetesFavoritos $ unPerro

    -diaDeSpaEjercicio
diaDeSpaEjercicio :: Ejercicio
diaDeSpaEjercicio unPerro = energiaLlena.regalar "Peine de Goma" (any 
($ unPerro) requisitosDiaDeSpa)

energiaLlena :: Perro -> Perro
sumarEnergia unPerro = energia unPerro == 100

type Requisito = Perro -> Bool
type Requisitos = Requisito
requisitosDiaDeSpa :: Requisitos
requisitosDiaDeSpa = [ permanece50Min , esRazaExtravagante ]

permanece50Min :: Perro -> Perro 
permanece50Min unPerro = (>=50).tiempoPermanencia unPerro

tiempoPermanencia :: Perro -> TiempoPermanencia
tiempoPermanencia (_, _, _, unTiempoPermanencia, _) = unTiempoPermanencia
