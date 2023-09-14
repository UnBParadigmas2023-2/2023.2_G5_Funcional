import Data.Char (ord)
import System.Random

data Barco = Galeao | Fragata | Jangada deriving (Show, Eq)


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
imprimirCelula NenhumBarco = putStr "~ "  -- Célula sem barco
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
    putStrLn "   A B C D E F G H I J K L"
    putStrLn "  -------------------------"
    mapM_ imprimirLinha (zip ['A'..] tabuleiro)
    putStrLn "  -------------------------"
    where
        imprimirLinha :: (Char, [Celula]) -> IO ()
        imprimirLinha (rotulo, linha) = do
            putStr [rotulo, ' ', '|']
            mapM_ imprimirCelula linha
            putStrLn "|"

main :: IO ()
main = do
    let meuTabuleiro = tabuleiroVazio
    imprimirTabuleiro meuTabuleiro
