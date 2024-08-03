-- Tipos.hs
module Tipos
    ( Investimento(..)
    , Simulacao(..)
    , SimulacaoDividendos(..)
    , Investivel(..)
    ) where

-- Definindo tipos algébricos
data Investimento = LCI { taxaLCI :: Double }
                  | CDB { taxaCDB :: Double }
                  deriving Show

data Simulacao = Simulacao
    { tipoInvestimento :: String
    , valorInicial :: String
    , meses :: String
    , aporteMensal :: String
    , rendimento :: Double
    } deriving Show

data SimulacaoDividendos = SimulacaoDividendos
    { nomeAcao :: String
    , cotacaoAtual :: String
    , dividendos12Meses :: String
    , valorMedioMensal :: String
    , valorNecessario :: Double
    } deriving Show

-- Definindo uma classe de tipo para investimentos
class Investivel a where
    calcularRendimento :: a -> Double -> Int -> Double -> Double



