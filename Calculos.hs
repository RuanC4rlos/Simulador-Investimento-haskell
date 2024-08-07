module Calculos where
import Tipos

-- Implementando a classe de tipo para LCI, CDB e Poupanca
instance Investivel Investimento where
    calcularRendimento (LCI t) valor meses aporteMensal =
        let valorTotal = valor + aporteMensal * fromIntegral meses
            rendimento = (valorTotal * (1 + t / 12) ^ fromIntegral meses) - valorTotal
        in rendimento

    calcularRendimento (CDB t) valor meses aporteMensal =
        let valorTotal = valor + aporteMensal * fromIntegral meses
            rendimento = valorTotal * (1 + t) ^ fromIntegral meses - valorTotal
        in rendimento
    
    calcularRendimento (Poupanca t) valor meses aporteMensal =
        let valorTotal = valor + aporteMensal * fromIntegral meses
            rendimento = valorTotal * (1 + t) ^ fromIntegral meses - valorTotal
        in rendimento

-- Função auxiliar para calcular valor necessário para dividendos
calcularValorNecessario :: Double -> Double -> Double -> Double
calcularValorNecessario cotacao dividendos valorMedioMensal =
    cotacao * dividendos * valorMedioMensal * 10