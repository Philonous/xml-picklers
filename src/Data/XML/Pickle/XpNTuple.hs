{-# LANGUAGE  TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings         #-}
module Data.XML.Pickle.XpNTuple where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Data.XML.Pickle
import qualified Data.Text as T


-- | Generates an xpNTuple function (max is 62 for GHC)

xpNTuple :: Int -> ExpQ
xpNTuple n = let debugText = stringE $ "xp" ++ show n ++ "Tuple"
                 def = do let getNames' start prefix =
                                let names = map (\i -> mkName $ prefix ++ show i) [start..n]
                                in (names
                                   , map VarP names, map varP names
                                   , map VarE names, map varE names
                                   )
                              getNames = getNames' 1
                              (xpsNames, xpsVarsP, xpsVarsPQ, xpsVarsE, xpsVarsEQ) = getNames "xp"
                              (tNames, tVarsP, tVarsPQ, tVarsE, tVarsEQ) = getNames "t"
                            
                              pickleTree' :: ExpQ
                              pickleTree' = let pickles = listE (map (\(xp, t) -> [| pickleTree $xp $t |])
                                                                      (zip xpsVarsEQ tVarsEQ))
                                                concatPickles = [| concat $pickles |]
                                            in lamE [tupP tVarsPQ] $ concatPickles

                              (rNames, rVarsP, rVarsPQ, rVarsE, rVarsEQ) = getNames' 0 "r"
                              (xNames, xVarsP, xVarsPQ, xVarsE, xVarsEQ) = getNames "x"
                              unpickleTree' = let text = stringE . show
                                                  unpickle i = [| tErr $(text i) $ getRest $ unpickleTree $(xpsVarsEQ !! (i-1)) $(rVarsEQ !! (i-1)) |]
                                                  pats i = tupP [xVarsPQ !! (i-1), rVarsPQ !! (i)]
                                                  binds = map (\i -> bindS (pats i) (unpickle i)) [1..(n-1)]

                                                  unpickleLast = [| tErr $(text n) $ unpickleTree $(xpsVarsEQ !! (n-1)) $(rVarsEQ !! (n-1)) |]
                                                  patsLast = xVarsPQ !! (n-1)
                                                  bindLast = bindS patsLast unpickleLast
                                                  returnAllX = noBindS $ [| return  $(tupE xVarsEQ) |]
                                              in lamE [(rVarsPQ !! 0)] $ (doE (binds ++ [bindLast, returnAllX]))
                          let body = [|
                                      T.pack $debugText <??>
                                      PU {pickleTree = $pickleTree'
                                         , unpickleTree = $unpickleTree'
                                         }
                                      |]
                          lamE xpsVarsPQ body
              in def

-- | generates xpNTuple where 6 < n < 62 (max of GHC)
xpTuples :: Int -> Q [Dec]
xpTuples i = sequence $ map xpTuple [7..i]
  where xpTuple n = funD (mkName $ "xp" ++ show n ++ "Tuple") [ clause [] (normalB $ xpNTuple n) [] ]
