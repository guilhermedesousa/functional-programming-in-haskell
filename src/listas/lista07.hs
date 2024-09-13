import Control.Monad.State
import Data.Maybe

data Produto = Produto { nome :: String,
                         preco :: Int } deriving (Show, Eq)

data Loja = Loja { caixa :: Int,
                   estoque :: [Produto] } deriving (Show, Eq)

comprar :: String -> State Loja (Maybe Produto)
comprar nomeProduto = state realizaCompra
    where
        realizaCompra :: Loja -> (Maybe Produto, Loja)
        realizaCompra lj =
            case findAndRemoveProduto $ estoque lj of
                Nothing -> (Nothing, lj)
                Just (p, novoEstoque) -> (Just p, Loja novoCaixa novoEstoque)
                    where novoCaixa = caixa lj + preco p
        findAndRemoveProduto :: [Produto] -> Maybe (Produto, [Produto])
        findAndRemoveProduto ps = go ps []
            where
                go [] _ = Nothing
                go (x:xs) acc
                    | nome x == nomeProduto = Just (x, reverse acc ++ xs)
                    | otherwise             = go xs (x : acc)

vender :: String -> Int -> State Loja Int
vender nomeProduto valor = state realizaVenda
    where
        realizaVenda :: Loja -> (Int, Loja)
        realizaVenda lj
            | valor <= caixa lj = (valor, Loja (caixa lj - valor) ((Produto nomeProduto valor) : estoque lj))
            | otherwise         = (0, lj)

type Cliente = State Loja Bool

vendeEspada :: Cliente
vendeEspada = do
    valorVendido <- vender "Espada" 10
    return $ valorVendido > 0

compraEscudo :: Cliente
compraEscudo = do
    maybeProduto <- comprar "Escudo"
    return $ isJust maybeProduto

shepard :: Cliente
shepard = do
    valorEspada <- vender "Espada" 10
    valorEscudo <- vender "Escudo" 5
    return $ valorEspada + valorEscudo > 0

frisk :: Cliente
frisk = do
    valorEspada <- vender "Espada" 10
    valorEscudo <- vender "Escudo" 5
    return $ valorEspada + valorEscudo == 15

loneWanderer :: Cliente
loneWanderer = do
    valorEspada <- vender "Espada" 10
    if valorEspada == 0
        then return False
        else isJust <$> comprar "Escudo"

dragonborn :: Cliente
dragonborn = do
    valor <- vender "Queijo" 3
    if valor > 0
        then dragonborn
        else return True

geralt :: Cliente
geralt = do
    valorEspadas <- sum <$> (sequenceA $ replicate 10 $ vender "Espada" 15)
    if valorEspadas >= (15 * 6)
        then isJust <$> comprar "Escudo"
        else return False