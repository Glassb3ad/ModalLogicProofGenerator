module PLlogicType where
data Proposition =
                    ATOM Int 
                   | NOT Proposition 
                   | AND Proposition Proposition 
                   | OR Proposition Proposition 
                   | IF Proposition Proposition
                   | EQ Proposition Proposition  
    deriving (Show, Eq) 