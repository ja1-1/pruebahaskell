import Data.Time.Clock 
import Data.List 
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq) -- para forzar la evaluación de una expresión

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante { -- usamos data para definir un nuevo tipo de dato
    idEstudiante :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime --usamos maybe para representar si el estudiante esta en la universidad o no
} deriving (Show, Read)

-- Registrar la entrada de un estudiante
registrarEntrada :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarEntrada id tiempo universidad =
    Estudiante id tiempo Nothing : universidad

-- Registrar la salida de un estudiante
registrarSalida :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida id tiempo universidad =
    map (\e -> if id == idEstudiante e then e { salida = Just tiempo } else e) universidad

-- Buscar estudiante por ID
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante id universidad =
    find (\e -> id == idEstudiante e && salida e == Nothing) universidad

-- Calcular el tiempo que un estudiante ha estado en la universidad
tiempoEnUniversidad :: Estudiante -> IO NominalDiffTime
tiempoEnUniversidad estudiante = do
    tiempoActual <- getCurrentTime
    return $ diffUTCTime tiempoActual (entrada estudiante)

-- Guardar info de los estudiantes en un archivo
guardaruniversidad :: [Estudiante] -> IO ()
guardaruniversidad universidad = do
    withFile "Universidad.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map show universidad))
    putStrLn "Datos guardados en Universidad.txt."

-- Cargar info de los estudiantes desde un archivo
cargaruniversidad :: IO [Estudiante]
cargaruniversidad = do
    contenido <- try (readFile "Universidad.txt") :: IO (Either IOError String)
    case contenido of
        Left _ -> return []  -- Si el archivo no existe, retorna una lista vacía
        Right datos -> return (map read (lines datos))

-- Mostrar un estudiante
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante id entrada salida) =
    "ID: " ++ id ++ ", Entrada: " ++ show entrada ++ ", Salida: " ++ maybe "El estudiante aun se encuentra en la universidad" show salida

-- Listar universidad
listaruniversidad :: [Estudiante] -> IO ()
listaruniversidad [] = putStrLn "No hay estudiantes en la universidad."
listaruniversidad universidad = do
    putStrLn "Lista de los estudiantes en la universidad:"
    mapM_ (putStrLn . mostrarEstudiante) universidad

-- Menú principal
main :: IO ()
main = do
    universidad <- cargaruniversidad
    putStrLn "Bienvenido al Sistema"
    cicloPrincipal universidad

cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal universidad = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de un estudiante"
    putStrLn "2. Registrar salida de un estudiante"
    putStrLn "3. Buscar estudiante por su ID"
    putStrLn "4. Listar estudiantes en la universidad"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID del estudiante:"
            id <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizados = registrarEntrada id tiempoActual universidad
            putStrLn $ "Estudiante con ID " ++ id ++ " registrado."
            guardaruniversidad universidadActualizados
            cicloPrincipal universidadActualizados
        
        "2" -> do
            putStrLn "Ingrese el ID del estudiante que sale:"
            id <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizados = registrarSalida id tiempoActual universidad
            putStrLn $ "Estudiante con ID " ++ id ++ " ha salido."
            guardaruniversidad universidadActualizados
            cicloPrincipal universidadActualizados
        
        "3" -> do
            putStrLn "Ingrese el ID del estudiante que va a buscar:"
            id <- getLine
            case buscarEstudiante id universidad of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnUniversidad estudiante
                    putStrLn $ "Estudiante con ID " ++ id ++ " está en la universidad." 
                    putStrLn $ "Tiempo en la universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en la universidad."
            cicloPrincipal universidad

        "4" -> do
            listaruniversidad universidad
            cicloPrincipal universidad

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida, intente de nuevo."
            cicloPrincipal universidad
