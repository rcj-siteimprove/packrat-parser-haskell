data Result v
  = Parsed v Derivs
  | NoParse

instance (Show v) => Show (Result v) where
  show (Parsed v _) = "Parsed " ++ show v
  show NoParse = "NoParse"

data Derivs = Derivs
  { dvAdditive :: Result Int,
    dvMultitive :: Result Int,
    dvPrimary :: Result Int,
    dvDecimal :: Result Int,
    dvChar :: Result Char
  }

pAdditive :: Derivs -> Result Int
pAdditive d = alt1
  where
    -- Additive <- Multitive '+' Additive
    alt1 = case dvMultitive d of
      Parsed vleft d' ->
        case dvChar d' of
          Parsed '+' d'' ->
            case dvAdditive d'' of
              Parsed vright d''' -> Parsed (vleft + vright) d'''
              _ -> alt2
          _ -> alt2
      _ -> alt2

    -- Additive <- Multitive
    alt2 = dvMultitive d

pMultitive :: Derivs -> Result Int
pMultitive d = alt1
  where
    -- Multitive <- Primary '*' Additive
    alt1 = case dvPrimary d of
      Parsed vleft d' ->
        case dvChar d' of
          Parsed '*' d'' ->
            case dvMultitive d'' of
              Parsed vright d''' -> Parsed (vleft * vright) d'''
              _ -> alt2
          _ -> alt2
      _ -> alt2

    -- Multitive <- Primary
    alt2 = dvPrimary d

pPrimary :: Derivs -> Result Int
pPrimary d = alt1
  where
    -- Primary <- '(' Additive ')'
    alt1 = case dvChar d of
      Parsed '(' d' ->
        case dvAdditive d' of
          Parsed v d'' ->
            case dvChar d'' of
              Parsed ')' d''' -> Parsed v d'''
              _ -> alt2
          _ -> alt2
      _ -> alt2

    -- Primary <- Decimal
    alt2 = dvDecimal d

pDecimal :: Derivs -> Result Int
pDecimal d = alt1
  where
    -- Decimal <- '0'
    alt1 = case dvChar d of
      Parsed '0' d' -> Parsed 0 d'
      _ -> alt2

    -- Decimal <- '1'
    alt2 = case dvChar d of
      Parsed '1' d' -> Parsed 1 d'
      _ -> alt3

    -- Decimal <- '2'
    alt3 = case dvChar d of
      Parsed '2' d' -> Parsed 2 d'
      _ -> alt4

    -- Decimal <- '3'
    alt4 = case dvChar d of
      Parsed '3' d' -> Parsed 3 d'
      _ -> alt5

    -- Decimal <- '4'
    alt5 = case dvChar d of
      Parsed '4' d' -> Parsed 4 d'
      _ -> alt6

    -- Decimal <- '5'
    alt6 = case dvChar d of
      Parsed '5' d' -> Parsed 5 d'
      _ -> alt7

    -- Decimal <- '6'
    alt7 = case dvChar d of
      Parsed '6' d' -> Parsed 6 d'
      _ -> alt8

    -- Decimal <- '7'
    alt8 = case dvChar d of
      Parsed '7' d' -> Parsed 7 d'
      _ -> alt9

    -- Decimal <- '8'
    alt9 = case dvChar d of
      Parsed '8' d' -> Parsed 8 d'
      _ -> alt10

    -- Decimal <- '9'
    alt10 = case dvChar d of
      Parsed '9' d' -> Parsed 9 d'
      _ -> NoParse

parse :: String -> Derivs
parse s = d
  where
    d = Derivs add mult prim dec chr
    add = pAdditive d
    mult = pMultitive d
    prim = pPrimary d
    dec = pDecimal d
    chr = case s of
      (c : s') -> Parsed c (parse s')
      [] -> NoParse

main :: IO ()
main = do
  input <- getLine
  print . dvAdditive . parse $ input
