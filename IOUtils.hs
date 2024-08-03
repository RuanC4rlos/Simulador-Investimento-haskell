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
exibirSimulacao (Simulacao tipo valor meses aporte rendimento) = do
    putStrLn $ "Tipo de Investimento: " ++ tipo
    putStrLn $ "Valor Inicial: R$ " ++ valor
    putStrLn $ "Meses: " ++ meses
    putStrLn $ "Aporte Mensal: R$ " ++ aporte
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



