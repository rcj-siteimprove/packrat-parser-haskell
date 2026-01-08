data Result v
  = Parsed v Derivs
  | NoParse

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
    alt1 = case dvMultitive d of
      Parsed vleft d' ->
        case dvChar d' of
          Parsed '+' d'' ->
            case dvAdditive d'' of
              Parsed vright d''' -> Parsed (vleft + vright) d'''
              _ -> alt2
          _ -> alt2
      _ -> alt2

    alt2 = dvMultitive d

-- pMultitive :: Derivs -> Result Int
-- pPrimary :: Derivs -> Result Int
-- pDecimal :: Derivs -> Result Int

main :: IO ()
main = putStrLn "Compiled: packrat-parser-haskell"
