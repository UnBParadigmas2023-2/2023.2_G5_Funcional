import Data.Char (ord)
import System.Random
import Control.Monad (foldM,when)
import System.IO (hFlush, stdout)
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)

-- Aqui
import qualified Data.Map as Map


instance Random Barco where
    randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
        (x, g') -> (toEnum x, g')
    random g = randomR (minBound, maxBound) g

data Barco = Galeao | Fragata | Jangada deriving (Show, Eq, Enum, Bounded, Ord)

valorPontos :: Barco -> Int
valorPontos Galeao = 3
valorPontos Fragata = 2
valorPontos Jangada = 1

-- Aqui
data Pontuacao = Pontuacao
    { pontosPorBarco :: Map.Map Barco Int
    } deriving (Show)

-- Função para calcular o tempo decorrido em segundos
tempoDecorrido :: UTCTime -> UTCTime -> Int
tempoDecorrido inicio atual = round $ realToFrac $ diffUTCTime atual inicio

-- Tempo Maximo em segundos para jogo baseado em tempo
tempoMaximoSegundos :: Int
tempoMaximoSegundos = 10

inicializarPontuacao :: Pontuacao
inicializarPontuacao = Pontuacao
    { pontosPorBarco = Map.fromList
        [ (Galeao, 0)
        , (Fragata, 0)
        , (Jangada, 0)
        ]
    }

afundarBarco :: Barco -> Pontuacao -> Pontuacao
afundarBarco barco pontuacao =
    let pontosDoBarco = valorPontos barco
        pontosAtuais = Map.lookup barco (pontosPorBarco pontuacao)
        novosPontos = case pontosAtuais of
            Just pontos -> pontos + pontosDoBarco
            Nothing -> pontosDoBarco
    in pontuacao { pontosPorBarco = Map.insert barco novosPontos (pontosPorBarco pontuacao) }
--


data Celula = NenhumBarco | Celula Bool Bool Bool Barco
type Tabuleiro = [[Celula]]

linhas = 12
colunas = 12

-- Função para a escolha do modo de jogo
escolherModoDeJogo :: IO ()
escolherModoDeJogo = do
    putStrLn "Escolha o modo de jogo:"
    putStrLn "1. Jogo baseado em tempo"
    putStrLn "2. Jogo baseado em turnos"

    escolha <- getLine

    let pontuacaoInicial = inicializarPontuacao
    tabuleiroFinal <- foldM (\tab _ -> adicionarBarcoAleatoriamente tab) tabuleiroVazio [1..10]
    inicio <- getCurrentTime

    case escolha of
        "1" -> do
            putStrLn "-----------------------------------------"
            putStrLn "Você escolheu jogar baseado em tempo."
            putStrLn "-----------------------------------------"
            putStrLn "Digite 'sair' a qualquer momento para encerrar o jogo.\n"
            loopTempo tabuleiroFinal pontuacaoInicial inicio

        "2" -> do
            putStrLn "-----------------------------------------"
            putStrLn "Você escolheu jogar baseado em turnos."
            putStrLn "-----------------------------------------"
            putStrLn "Digite 'sair' a qualquer momento para encerrar o jogo.\n"
            loopTurnos tabuleiroFinal pontuacaoInicial 0

        _ -> do
            putStrLn "Opção inválida. Por favor, escolha 1 ou 2."
            escolherModoDeJogo

celulaVazia :: Celula
celulaVazia = NenhumBarco

tabuleiroVazio :: Tabuleiro
tabuleiroVazio = replicate linhas (replicate colunas celulaVazia)

imprimirCelula :: Celula -> IO ()
imprimirCelula NenhumBarco = putStr "~ "
imprimirCelula (Celula temNavio foiAtacada afundado tipoBarco) = do
    if foiAtacada
        then if temNavio
            then do
                putStr (case tipoBarco of
                    Galeao -> if afundado then "g" else "G"
                    Fragata -> if afundado then "f" else "F"
                    Jangada -> if afundado then "j" else "J")
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

marcarCelula :: Tabuleiro -> Char -> String -> Tabuleiro
marcarCelula tabuleiro linha colunaStr =
    let coluna = read colunaStr :: Int
        linhaIndex = ord linha - ord 'A'
        celulaAtual = (tabuleiro !! linhaIndex) !! (coluna - 1)
        novaCelula = case celulaAtual of
            Celula temNavio _ _ tipoBarco -> Celula temNavio True True tipoBarco
            _ -> celulaAtual
        novaLinha = take (coluna - 1) (tabuleiro !! linhaIndex) ++ [novaCelula] ++ drop coluna (tabuleiro !! linhaIndex)
    in take linhaIndex tabuleiro ++ [novaLinha] ++ drop (linhaIndex + 1) tabuleiro

adicionarBarcoAleatoriamente :: Tabuleiro -> IO Tabuleiro
adicionarBarcoAleatoriamente tabuleiro = do
    linha <- randomRIO ('A', 'L')
    coluna <- randomRIO (1, 12)
    barco <- randomIO
    let linhaIndex = ord linha - ord 'A'
    let novaCelula = Celula True False False (if barco then Galeao else Fragata)
    let linhaAtual = tabuleiro !! linhaIndex
    let novaLinha = take (coluna - 1) linhaAtual ++ [novaCelula] ++ drop coluna linhaAtual
    return $ take linhaIndex tabuleiro ++ [novaLinha] ++ drop (linhaIndex + 1) tabuleiro

mostrarCoordenadasBarcos :: Tabuleiro -> IO ()
mostrarCoordenadasBarcos tabuleiro = do
    let coordenadas = [(lin, col) | lin <- ['A'..'L'], col <- [1..12]]
    let coordenadasBarcos = filter (\(lin, col) -> case obterCelula tabuleiro lin (show (col :: Int)) of
                                                  Celula True _ _ _ -> True
                                                  _ -> False) coordenadas
    mapM_ (\(lin, col) -> putStrLn (lin : ' ' : show (col :: Int))) coordenadasBarcos

revelarTabuleiro :: Tabuleiro -> Tabuleiro
revelarTabuleiro tabuleiro = map (map revelarCelula) tabuleiro
  where
    revelarCelula :: Celula -> Celula
    revelarCelula (Celula temNavio _ afundado tipoBarco) = Celula temNavio True afundado tipoBarco
    revelarCelula celula = celula

main :: IO ()
main = do
    putStrLn "------------------------------------------"
    putStrLn "\tBem Vindo ao Batalha Naval:"
    putStrLn "------------------------------------------"

    escolherModoDeJogo

loopTempo :: Tabuleiro -> Pontuacao -> UTCTime -> IO ()
loopTempo tabuleiro pontuacao inicio = do
    putStrLn "Digite a coordenada (linha coluna) que deseja verificar (por exemplo, A 1, onde 'A' é a linha e '1' a coluna);\nOu digite 'revelar' para mostrar as coordenadas dos barcos;\nOu 'sair' para finalizar a partida:"
    hFlush stdout
    atual <- getCurrentTime
    let segundosDecorridos = tempoDecorrido inicio atual
    putStrLn "=============================================================="
    putStrLn ("Tempo decorrido: " ++ show segundosDecorridos ++ " segundos")
    putStrLn "=============================================================="

    when (segundosDecorridos < tempoMaximoSegundos) $ do
        input <- getLine
        if input == "sair"
            then putStrLn "Jogo encerrado."
            else if input == "revelar"
                then do
                    putStrLn "Coordenadas dos barcos:"
                    mostrarCoordenadasBarcos tabuleiro
                    --loop tabuleiro pontuacao


                    let tabuleiroRevelado = revelarTabuleiro tabuleiro
                    putStrLn "Tabuleiro revelado:"
                    imprimirTabuleiro tabuleiroRevelado
                    loopTempo tabuleiroRevelado pontuacao inicio

                else let coordenadas = words input in
                    if length coordenadas /= 2
                        then do
                            putStrLn "\tEntrada inválida. Digite a coordenada no formato correto. (por exemplo, A 1, onde 'A' é a linha e '1' a coluna)\n"
                            hFlush stdout
                            loopTempo tabuleiro pontuacao inicio
                        else do
                            let [linha, colunaStr] = coordenadas
                            let celula = obterCelula tabuleiro (head linha) colunaStr

                            case celula of
                                Celula True _ False tipoBarco -> do
                                    let novoTabuleiro = marcarCelula tabuleiro (head linha) colunaStr
                                    imprimirTabuleiro novoTabuleiro
                                    putStrLn ("\tNa coordenada " ++ linha ++ " " ++ colunaStr ++ " está o barco: " ++ show tipoBarco ++ "\n")
                                    hFlush stdout
                                    let novaPontuacao = afundarBarco tipoBarco pontuacao
                                    let somaPontuacao = sum (Map.elems (pontosPorBarco novaPontuacao))
                                    putStrLn ("============================================================================")
                                    putStrLn ("Sua Pontuação(Galeão vale 3, Fragata 2 e Jangada 1): " ++ show somaPontuacao)
                                    putStrLn ("============================================================================")
                                    loopTempo novoTabuleiro novaPontuacao inicio
                                Celula True _ True _ -> do
                                        putStrLn "Você já atingiu este barco!"
                                        loopTempo tabuleiro pontuacao inicio -- ou loopTurnos, dependendo da função
                                _ -> do
                                    putStrLn ("-----------------------------------------------------------------")
                                    putStrLn ("\tNa coordenada " ++ linha ++ " " ++ colunaStr ++ " não há barco.")
                                    if verificarBarcoProximo tabuleiro (coordsParaIndices (unwords coordenadas))
                                        then putStrLn "\tMas há um barco próximo!"
                                        else return ()
                                    putStrLn ("-----------------------------------------------------------------")
                                    let standPontuacao = sum (Map.elems (pontosPorBarco pontuacao))
                                    putStrLn ("============================================================================")
                                    putStrLn ("Sua Pontuação(Galeão vale 3, Fragata 2 e Jangada 1): " ++ show standPontuacao)
                                    putStrLn ("============================================================================")
                                    hFlush stdout
                                    loopTempo tabuleiro pontuacao inicio
    when (segundosDecorridos >= tempoMaximoSegundos) $ do
        fimDeJogoTempo

loopTurnos :: Tabuleiro -> Pontuacao -> Int -> IO ()
loopTurnos tabuleiro pontuacao turnos = do
    putStrLn ("Digite a coordenada (linha coluna) que deseja verificar (por exemplo, A 1, onde 'A' é a linha e '1' a coluna)\n"
             ++ " Ou digite 'revelar' para mostrar as coordenadas dos barcos\n"
             ++ " Ou 'sair' para finalizar a partida:\n")
    putStrLn ("==============================================================")
    putStrLn ("Turnos Restantes: " ++ show (10 - turnos))
    putStrLn ("==============================================================")
    hFlush stdout

    if turnos >= 10
        then do
            fimDeJogoTurnos
        else do
            input <- getLine
            if input == "sair"
                then do
                    putStrLn "Jogo encerrado."
                else if input == "revelar"
                    then do
                        putStrLn "Coordenadas dos barcos:"
                        mostrarCoordenadasBarcos tabuleiro

                        let tabuleiroRevelado = revelarTabuleiro tabuleiro
                        putStrLn "Tabuleiro revelado:"
                        imprimirTabuleiro tabuleiroRevelado
                        loopTurnos tabuleiroRevelado pontuacao turnos
                    else let coordenadas = words input in
                        if length coordenadas /= 2
                            then do
                                putStrLn "\tEntrada inválida. Digite a coordenada no formato correto. (por exemplo, A 1, onde 'A' é a linha e '1' a coluna)\n"
                                hFlush stdout
                                loopTurnos tabuleiro pontuacao turnos
                            else do
                                let [linha, colunaStr] = coordenadas
                                let celula = obterCelula tabuleiro (head linha) colunaStr

                                case celula of
                                    Celula True _ False tipoBarco -> do
                                        let novoTabuleiro = marcarCelula tabuleiro (head linha) colunaStr
                                        imprimirTabuleiro novoTabuleiro
                                        putStrLn ("\tNa coordenada " ++ linha ++ " " ++ colunaStr ++ " está o barco: " ++ show tipoBarco ++ "\n")
                                        hFlush stdout
                                        let novaPontuacao = afundarBarco tipoBarco pontuacao
                                        let somaPontuacao = sum (Map.elems (pontosPorBarco novaPontuacao))
                                        putStrLn ("============================================================================")
                                        putStrLn ("Sua Pontuação(Galeão vale 3, Fragata 2 e Jangada 1): " ++ show somaPontuacao)
                                        putStrLn ("============================================================================")
                                        loopTurnos novoTabuleiro novaPontuacao (turnos + 1)
                                    Celula True _ True _ -> do
                                        putStrLn "Você já atingiu este barco!"
                                        loopTurnos tabuleiro pontuacao (turnos + 1)
                                    _ -> do
                                        putStrLn ("-----------------------------------------------------------------")
                                        putStrLn ("\tNa coordenada " ++ linha ++ " " ++ colunaStr ++ " não há barco.")
                                        if verificarBarcoProximo tabuleiro (coordsParaIndices (unwords coordenadas))
                                            then putStrLn "\tMas há um barco próximo!"
                                            else return ()
                                        putStrLn ("-----------------------------------------------------------------")
                                        let standPontuacao = sum (Map.elems (pontosPorBarco pontuacao))
                                        putStrLn ("============================================================================")
                                        putStrLn ("Sua Pontuação(Galeão vale 3, Fragata 2 e Jangada 1): " ++ show standPontuacao)
                                        putStrLn ("============================================================================")
                                        hFlush stdout
                                        loopTurnos tabuleiro pontuacao (turnos + 1)


coordsParaIndices :: String -> (Int, Int)
coordsParaIndices (letra:numero) = (ord letra - ord 'A', read numero - 1)

verificarBarcoProximo :: Tabuleiro -> (Int, Int) -> Bool
verificarBarcoProximo tabuleiro (i, j) = any temBarco vizinhos
    where
        offsets = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
        vizinhos = [(i + x, j + y) | (x, y) <- offsets, i + x >= 0, j + y >= 0, i + x < linhas, j + y < colunas]
        temBarco (x, y) = case tabuleiro !! x !! y of
            Celula True _ _ _ -> True
            _               -> False

fimDeJogoTempo :: IO ()
fimDeJogoTempo = do
    putStrLn "--------------------------------"
    putStrLn "Fim de jogo! - Seu Tempo Acabou"
    putStrLn "--------------------------------"

fimDeJogoTurnos :: IO ()
fimDeJogoTurnos = do
    putStrLn "------------------------------------"
    putStrLn "Fim de jogo! - Seus Turnos Acabaram"
    putStrLn "------------------------------------"