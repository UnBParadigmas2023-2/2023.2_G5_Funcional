import Data.Char (ord)
import System.Random
import Control.Monad (foldM)
import System.IO (hFlush, stdout)
import Control.Applicative ((<|>))



instance Random Barco where
    randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
        (x, g') -> (toEnum x, g')
    random g = randomR (minBound, maxBound) g

data Barco = Galeao | Fragata | Jangada deriving (Show, Eq, Enum, Bounded)

valorPontos :: Barco -> Int
valorPontos Galeao = 3
valorPontos Fragata = 2
valorPontos Jangada = 1

data Celula = NenhumBarco | Celula Bool Bool Barco  
type Tabuleiro = [[Celula]]

linhas = 12
colunas = 12

celulaVazia :: Celula
celulaVazia = NenhumBarco

tabuleiroVazio :: Tabuleiro
tabuleiroVazio = replicate linhas (replicate colunas celulaVazia)

imprimirCelula :: Celula -> IO ()
imprimirCelula NenhumBarco = putStr "~ "  
imprimirCelula (Celula temNavio foiAtacada tipoBarco) = do
    if foiAtacada
        then if temNavio
            then do
                putStr (case tipoBarco of
                    Galeao -> "G"
                    Fragata -> "F"
                    Jangada -> "J")
                let valorBarco = valorPontos tipoBarco
                putStr ("(" ++ show valorBarco ++ ")")
            else putStr "X"  
        else putStr "~ "  

imprimirTabuleiro :: Tabuleiro -> IO ()
imprimirTabuleiro tabuleiro = do
    putStrLn "   1 2 3 4 5 6 7 8 9 10 11 12"
    putStrLn "  -------------------------"
    mapM_ imprimirLinha (zip ['A'..] tabuleiro)
    putStrLn "  -------------------------"
    where
        imprimirLinha :: (Char, [Celula]) -> IO ()
        imprimirLinha (rotulo, linha) = do
            putStr [rotulo, ' ', '|']
            mapM_ imprimirCelula linha
            putStrLn "|"

obterCelula :: Tabuleiro -> Char -> String -> Celula
obterCelula tabuleiro linha colunaStr =
    let coluna = read colunaStr :: Int
        linhaIndex = ord linha - ord 'A'
    in (tabuleiro !! linhaIndex) !! (coluna - 1)

adicionarBarcoAleatoriamente :: Tabuleiro -> IO Tabuleiro
adicionarBarcoAleatoriamente tabuleiro = do
    linha <- randomRIO ('A', 'L') 
    coluna <- randomRIO (1, 12)  
    barco <- randomIO  
    let linhaIndex = ord linha - ord 'A'
    let novaCelula = Celula True False (if barco then Galeao else Fragata)  
    let linhaAtual = tabuleiro !! linhaIndex
    let novaLinha = take (coluna - 1) linhaAtual ++ [novaCelula] ++ drop coluna linhaAtual
    return $ take linhaIndex tabuleiro ++ [novaLinha] ++ drop (linhaIndex + 1) tabuleiro


mostrarCoordenadasBarcos :: Tabuleiro -> IO ()
mostrarCoordenadasBarcos tabuleiro = do
    let coordenadas = [(lin, col) | lin <- ['A'..'L'], col <- [1..12]]
    let coordenadasBarcos = filter (\(lin, col) -> case obterCelula tabuleiro lin (show col) of
                                                          Celula True _ _ -> True
                                                          _ -> False) coordenadas
    mapM_ (\(lin, col) -> putStrLn (lin : ' ' : show col)) coordenadasBarcos
    
main :: IO ()
main = do
        
    tabuleiroFinal <- foldM (\tab _ -> adicionarBarcoAleatoriamente tab) tabuleiroVazio [1..10]
        
    putStrLn "Bem Vindo ao Batalha Naval:"
    imprimirTabuleiro tabuleiroFinal
    
    putStrLn "Digite 'sair' a qualquer momento para encerrar o jogo."
    loop tabuleiroFinal
    
loop :: Tabuleiro -> IO ()
loop tabuleiro = do
    putStrLn "Digite a coordenada (linha coluna) que deseja verificar (por exemplo, A 1, onde 'A' é a linha e '1' a coluna), ou digite 'revelar' para mostrar as coordenadas dos barcos ou 'sair' para finalizar a partida:"
    input <- getLine
    if input == "sair"
        then putStrLn "Jogo encerrado."
        else if input == "revelar"
            then do
                putStrLn "Coordenadas dos barcos:"
                mostrarCoordenadasBarcos tabuleiro
                loop tabuleiro
            else do
                let coordenadas = words input  
                if length coordenadas /= 2
                    then do 
                        putStrLn "Entrada inválida. Digite a coordenada no formato correto. (por exemplo, A 1, onde 'A' é a linha e '1' a coluna)"
                        hFlush stdout
                        loop tabuleiro
                        
                    else do
                        let [linha, colunaStr] = coordenadas
                            celula = obterCelula tabuleiro (head linha) colunaStr
                            barco = case celula of
                                Celula True _ tipoBarco -> show tipoBarco
                                _ -> "Nenhum barco"
                        putStrLn ("Na coordenada " ++ linha ++ " " ++ colunaStr ++ " está o barco: " ++ barco)
                        hFlush stdout
                        loop tabuleiro
    
    