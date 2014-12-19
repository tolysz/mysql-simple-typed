{-# Language TemplateHaskell #-}

module Database.MySQL.Simple.TypedQuery
( genJsonQuery
, genTypedQuery
, TQ.genUncurry
, TQ.TypedQuery(..)
, S.Query
)
where

import qualified Database.MySQL.Base         as S (Connection)
import qualified Database.MySQL.Simple.Types as S (fromQuery)
import qualified Database.MySQL.Simple       as S (query, query_, execute, execute_, Only(..), In(..), connect, Query)
import Database.MySQL.Simple.DBmore()
import qualified Database.TypedQuery.Types as TQ
import Language.Haskell.TH.Syntax (Q, Exp, Lift(..))
import Data.ByteString.UTF8 (toString)
import Prelude ( (.))


-- Move this somewhere
instance Lift S.Query where
  lift = lift . toString . S.fromQuery

instance TQ.RunDB S.Query where
  rdquery  _   = 'S.query
  rdquery_ _   = 'S.query_
  rdexecute_ _ = 'S.execute_
  rdexecute  _ = 'S.execute
  rdin       _ = 'S.In
  rdonly     _ = 'S.Only
  rdconn     _ = ''S.Connection

genJsonQuery :: TQ.TypedQuery S.Query -> Q Exp
genJsonQuery = TQ.genJsonQuery

genTypedQuery :: TQ.TypedQuery S.Query -> Q Exp
genTypedQuery = TQ.genTypedQuery
