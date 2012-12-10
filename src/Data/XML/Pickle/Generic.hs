module Data.XML.Pickle.Generic
       ( xpGeneric
       )

       where

import Control.Applicative ((<$>))
import Data.Data
import Data.XML.Pickle
import Data.XML.Types
import qualified Data.Text as Text
import Control.Monad.State
import Control.Monad.Trans

constrs x = map constrFields. dataTypeConstrs $ dataTypeOf x

text :: Show a => a -> Text.Text
text = Text.pack . show

toElement :: Data a => Maybe Text.Text -> a -> Element
toElement ns x = case toNode ns x of
    NodeElement e -> e
    c@(NodeContent _) -> Element "val" [] [c]

toNode :: Data a => Maybe Text.Text -> a -> Node
toNode ns x = case constrRep c of
                AlgConstr _ -> NodeElement $ Element
                                 (Name (text c)
                                       ns
                                       Nothing)
                                 []
                                 (NodeElement <$> mapElems)
                _ -> NodeContent $ ContentText (text c)
  where
    c  = toConstr x
    fs = Text.pack <$> constrFields c
    mapElems = if null fs then gmapQ (toElement ns)  x
                          else zipWith mapElem fs $ gmapQ (toNode ns) x
    mapElem n c = (Element (Name n ns Nothing) [] [c] )

upe :: String -> Either UnpickleError a
upe e = Left $ ErrorMessage (Text.pack e)

fromNode :: Data a => Maybe Text.Text -> Node -> Either UnpickleError a
fromNode ns n = fromNode'
  where
    dt = let ~(Right x) = fromNode' in dataTypeOf x
    fromNode' = case dataTypeRep dt of
                        AlgRep as ->
                            case n of
                                NodeElement e -> fromElement ns e
                                _ -> upe $ "Algebraic type with NodeContent"
                        _ -> case n of
                                    NodeContent (ContentText t) ->
                                        case readConstr dt $ Text.unpack t of
                                            Nothing -> upe "Couldn't read constr"
                                            Just c -> Right $ fromConstr c
                                    n -> upe $ "Non-algebraic type" ++ show dt
                                               ++ " with Element " ++ show n



fromElement :: Data a => Maybe Text.Text -> Element -> Either UnpickleError a
fromElement ns e = fE
  where
    dt = let ~(Right x) = fE in dataTypeOf x
    fE = case dataTypeRep dt of
        AlgRep _ -> fromElement' ns e
        _ -> case e of
            Element (Name "val" ns _) [] cs -> case flattenContent cs of
                [c] -> fromNode ns c
                _  -> upe "Too many nodes"
            e  -> upe $ "Missing wrapper element " ++ show dt ++ " / " ++ show e


fromElement' :: Data a => Maybe Text.Text -> Element -> Either UnpickleError a
fromElement' ns (Element (Name lp ns' pf) [] cs)
    | ns == ns' = fromElement''
    | otherwise = upe $ "namespace doesn't match: "
                       ++ show ns ++ " /= " ++ show ns'
  where
    dt = let ~(Right x) = fromElement'' in dataTypeOf x
    fromElement'' = constr >>= \c -> case constrFields c of
        [] -> fst <$> runStateT (fromConstrM nextN c) cs
        fs -> fst <$> runStateT (fromConstrM nextF c) (cs, Text.pack <$> fs)
    constr = case readConstr dt $ Text.unpack lp of
        Nothing -> upe $ "Could not read constr " ++ show lp ++ " / " ++ show dt
        Just r -> Right r
    nextN :: Data a => StateT [Node] (Either UnpickleError) a
    nextN = do
        xs' <- get
        case xs' of
            [] -> lift . upe $ "Not enough elements (N)" ++ show dt
            (x:xs) -> do
                put xs
                case x of
                    NodeElement e -> lift $ fromElement ns e
                    c -> lift . upe $ "unexpected NodeContent " ++ show c ++ " / " ++ show dt

    nextF :: Data a => StateT ([Node], [Text.Text]) (Either UnpickleError) a
    nextF = do
        rs <- get
        x <- case rs of
            (x:xs, f:fs) -> do
                put (xs, fs)
                case x of
                    NodeElement (Element name@(Name lp ns'' _) _ [c])
                        | (lp == f) && (ns'' == ns) -> return c
                        | otherwise -> lift . upe $ "name doesn't match " ++ show name
                    n -> lift . upe $ "Wrong node type" ++ show n ++ " / "
                                        ++ show dt
            _ -> lift $ upe "not enough elements (F)"
        lift $ fromNode ns x
fromElement' _ _ = upe "Element had attributes"

-- | Generate a pickler for any Data.Data.Data instance.
xpGeneric :: Data a => Maybe Text.Text -> PU Element a
xpGeneric ns = ("xpGeneric","") <?+>
               PU { pickleTree = toElement ns
                  , unpickleTree = \e -> case fromElement ns e of
                      Left e -> Left e
                      Right a -> Right (a, (Nothing, True))
               }
