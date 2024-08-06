module IOUtils where
import Tipos
import Calculos
import Text.Printf
import System.Process

-- Função para exibir o histórico
exibirHistorico :: [Simulacao] -> [SimulacaoDividendos] -> IO ()
exibirHistorico historico historicoDividendos = do
    putStrLn "\n*-----------------------------------------------*"
    putStrLn "|             HISTÓRICO DE SIMULAÇÕES           |"
    putStrLn "*-----------------------------------------------*"
    mapM_ exibirSimulacao (reverse historico)
    mapM_ exibirSimulacaoDividendos (reverse historicoDividendos)
    
-- Função para exibir uma simulação formatada
exibirSimulacao :: Simulacao -> IO ()
exibirSimulacao (Simulacao tipo valor meses aporte rendimento totalInvestido) = do
    putStrLn $ "Tipo de Investimento: " ++ tipo
    putStrLn $ "Valor Inicial: R$ " ++ valor
    putStrLn $ "Meses: " ++ meses
    putStrLn $ "Aporte Mensal: R$ " ++ aporte
    putStrLn $ "Total Investido: R$ " ++ printf "%.2f" totalInvestido 
    putStrLn $ "Rendimento: R$ " ++ printf "%.2f" rendimento
    putStrLn "-----------------------------------------------"

exibirSimulacaoDividendos :: SimulacaoDividendos -> IO ()
exibirSimulacaoDividendos (SimulacaoDividendos nome cotacao dividendos valorMedio valorNecessario) = do
    putStrLn $ "Nome da Ação: " ++ nome
    putStrLn $ "Cotação Atual: R$ " ++ cotacao
    putStrLn $ "Dividendos (últimos 12 meses): R$ " ++ dividendos
    putStrLn $ "Valor Médio Mensal Desejado: R$ " ++ valorMedio
    putStrLn $ "Valor Necessário para Investimento: R$ " ++ printf "%.2f" valorNecessario
    putStrLn "-----------------------------------------------"

-- Função para limpar a tela
clearScreen :: IO ()
clearScreen = do
    _ <- system "clear"  -- Use "cls" no Windows
    return ()



-- Função para verificar se uma string representa um número válido
isValidDouble :: String -> Bool
isValidDouble s = case reads s :: [(Double, String)] of
    [(n, "")] -> n >= 0
    _         -> False

-- Função para ler e validar um número positivo (pode ser decimal)
readPositiveDouble :: String -> IO Double
readPositiveDouble prompt = do
    putStrLn prompt
    input <- getLine
    if isValidDouble input
       then return (read input :: Double)
       else do
           putStrLn "Entrada inválida. Por favor, digite um número válido."
           readPositiveDouble prompt

-- Função para verificar se uma string representa um número inteiro válido
isValidInt :: String -> Bool
isValidInt s = case reads s :: [(Int, String)] of
    [(n, "")] -> n >= 0
    _         -> False

-- Função para ler e validar um número inteiro positivo
readPositiveInt :: String -> IO Int
readPositiveInt prompt = do
    putStrLn prompt
    input <- getLine
    if isValidInt input
       then return (read input :: Int)
       else do
           putStrLn "Entrada inválida. Por favor, digite um número válido."
           readPositiveInt prompt
