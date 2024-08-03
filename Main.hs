module Main where

import Tipos
import Calculos
import IOUtils
import Text.Printf
import Data.Char (isDigit)

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

-- Função auxiliar principal com histórico
mainAux :: [Simulacao] -> [SimulacaoDividendos] -> IO ()
mainAux historico historicoDividendos = do
    putStrLn "\n*-----------------------------------------------*"
    putStrLn "|           SIMULADOR DE INVESTIMENTOS          |"
    putStrLn "*-----------------------------------------------*"
    putStrLn "Escolha o tipo de investimento:"
    putStrLn "   1- Simular na Poupança"
    putStrLn "   2- Investir em Bancos (CDB)"
    putStrLn "   3- Investir em Imóveis (LCI)"
    putStrLn "   4- Simular dividendos em Ações"
    putStrLn "   5- Exibir Histórico de Simulações"
    putStrLn "   0- Encerrar Simulação"
    putStrLn "OPÇÃO:"
    escolha <- getLine
    case escolha of
        "1" -> do
            clearScreen
            putStrLn "*---------------------------------*"
            putStrLn "|       SIMULAR NA POUPANÇA       |"
            putStrLn "*---------------------------------*"

            valorInicial <- readPositiveDouble "Informe o valor inicial na poupança:"
            meses <- readPositiveInt "Informe o número de meses de investimento:"
            aporteMensal <- readPositiveDouble "Informe o aporte mensal:"

            let rendimentoPoupanca = calcularRendimentoPoupanca valorInicial meses aporteMensal

            putStrLn $ "Rendimento na poupança: R$ " ++ printf "%.2f" rendimentoPoupanca
            putStrLn "-----------------------------------------------------------\n"

            let simulacaoPoupanca = Simulacao "Poupança" (show valorInicial) (show meses) (show aporteMensal) rendimentoPoupanca
                historicoAtualizado = simulacaoPoupanca : historico
            mainAux historicoAtualizado historicoDividendos

        "2" -> do
            clearScreen
            putStrLn "*--------------------------------------*"
            putStrLn "|       INVESTIR EM BANCOS (CDB)       |"
            putStrLn "*--------------------------------------*"
            
            valorInicial <- readPositiveDouble "Informe o valor do investimento em CDB:"
            taxa <- readPositiveDouble "Informe a taxa mensal (em decimal, por exemplo, 0.01 para 1%):"
            meses <- readPositiveInt "Informe o número de meses de investimento:"
            aporteMensal <- readPositiveDouble "Informe o aporte mensal:"

            let rendimentoCDB = calcularRendimento (CDB taxa) valorInicial meses aporteMensal

            putStrLn $ "Rendimento no CDB: R$ " ++ printf "%.2f" rendimentoCDB
            putStrLn "-----------------------------------------------------------\n"

            let simulacaoCDB = Simulacao "CDB" (show valorInicial) (show meses) (show aporteMensal) rendimentoCDB
                historicoAtualizado = simulacaoCDB : historico
            mainAux historicoAtualizado historicoDividendos

        "3" -> do
            clearScreen
            putStrLn "*---------------------------------------*"
            putStrLn "|       INVESTIR EM IMÓVEIS (LCI)       |"
            putStrLn "*---------------------------------------*"
            
            valorInicial <- readPositiveDouble "Informe o valor do investimento em LCI:"
            meses <- readPositiveInt "Informe o número de meses de investimento:"
            aporteMensal <- readPositiveDouble "Informe o aporte mensal:"

            let investimentoEscolhido = LCI 0.08  -- LCI com taxa de 8%
                rendimentoLCI = calcularRendimento investimentoEscolhido valorInicial meses aporteMensal

            putStrLn $ "Rendimento na LCI: R$ " ++ printf "%.2f" rendimentoLCI
            putStrLn "-----------------------------------------------------------\n"

            let simulacaoLCI = Simulacao "LCI" (show valorInicial) (show meses) (show aporteMensal) rendimentoLCI
                historicoAtualizado = simulacaoLCI : historico
            mainAux historicoAtualizado historicoDividendos

        "4" -> do
            clearScreen
            putStrLn "*-----------------------------------------*"
            putStrLn "|       SIMULAR DIVIDENDOS EM AÇÕES       |"
            putStrLn "*-----------------------------------------*"
            
            valorMedioMensal <- readPositiveDouble "Informe o valor médio mensal que você gostaria de ter:"
            putStrLn "Informe o nome do ativo que você gostaria de simular:"
            nome <- getLine
            cotacao <- readPositiveDouble "Informe a cotação atual da ação:"
            dividendos <- readPositiveDouble "Informe os dividendos (últimos 12 meses) da ação:"

            let valorNecessario = calcularValorNecessario cotacao dividendos valorMedioMensal

            putStrLn $ "Para ter uma renda média mensal de R$ " ++ printf "%.2f" valorMedioMensal
                ++ " em " ++ nome ++ ", você precisa investir R$ "
                ++ printf "%.2f" valorNecessario
            putStrLn "-----------------------------------------------------------\n"

            let simulacaoDividendos = SimulacaoDividendos nome (show cotacao) (show dividendos) (show valorMedioMensal) valorNecessario
                historicoDividendosAtualizado = simulacaoDividendos : historicoDividendos
            mainAux historico historicoDividendosAtualizado

        -- Adicione as opções restantes aqui
        "5" -> do
            exibirHistorico historico historicoDividendos
            mainAux historico historicoDividendos
        "0" -> putStrLn "Encerrando Simulação."
        _   -> do
            putStrLn "Opção inválida. Por favor, selecione uma opção válida."
            mainAux historico historicoDividendos

main :: IO ()
main = mainAux [] []







