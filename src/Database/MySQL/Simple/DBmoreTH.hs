{-# LANGUAGE TupleSections
           , OverloadedStrings
           , ScopedTypeVariables
           , NoMonomorphismRestriction
           , BangPatterns
           , TemplateHaskell
           , CPP
  #-}
module Database.MySQL.Simple.DBmoreTH
( qp
, qr
)
where

import Database.MySQL.Base ()
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.Param
import Database.MySQL.Simple.Result
import Database.MySQL.Simple.QueryResults

import Language.Haskell.TH.Syntax
import Control.Monad

import Prelude (map, (!!), (<), (-), (.) ,($), head, otherwise, zipWith3, Int, toInteger)

-- maybe add case 0 + 1 and make it being generated

clap x =
#if MIN_VERSION_template_haskell(2,10,0)
   AppT (ConT ''Param) (VarT x)
#else
   ClassP (''Param) [VarT x]
#endif

qp :: Int -> Q [Dec]
qp k =  do
    ns <- replicateM k (newName "a")
    let pre = map clap ns
    return [ InstanceD pre (loop k ns) [fun ns] ]
       where
         loop 0 ns = AppT (TupleT k) (VarT (head ns))
         loop i ns | i < k = AppT (loop (i-1) ns ) (VarT (ns !! i))
                   | otherwise = AppT (ConT ''QueryParams) (loop (i-1) ns )
         fun ns = FunD ('renderParams) [ Clause [TupP (map VarP ns)] (NormalB $ ListE $ map (AppE (VarE 'render) . VarE) ns ) [] ] 

qr :: Int -> Q [Dec]
qr k =  do
    nsa <- replicateM k (newName "a")
    nsv <- replicateM k (newName "v")
    nsf <- replicateM k (newName "f")
    fs <- newName "fs"
    vs <- newName "vs"

    let pre = map (\x -> clap (''Result) [VarT x] ) nsa
    return [ InstanceD pre (loop k nsa) [fun nsa nsf nsv fs vs] ]
       where
         loop 0 ns = AppT (TupleT k) (VarT (head ns))
         loop i ns | i < k = AppT (loop (i-1) ns ) (VarT (ns !! i))
                   | otherwise = AppT (ConT ''QueryResults) (loop (i-1) ns )
         fun nsa nsf nsv fs vs= FunD ('convertResults) 
                             [ Clause [ListP (map VarP nsf), ListP (map VarP nsv)] 
                                  (NormalB $ TupE $ map VarE nsa )
                                     (zipWith3 (\a f v -> ValD (BangP (VarP a)) (NormalB (AppE (AppE (VarE 'convert) (VarE f)) (VarE v))) [] ) nsa nsf nsv)
                             , Clause [VarP fs,VarP vs] (NormalB (AppE (AppE (AppE (VarE 'convertError) (VarE fs)) (VarE vs)) (LitE $ IntegerL $ toInteger k))) []
                             ]
