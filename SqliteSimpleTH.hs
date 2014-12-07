{-# LANGUAGE TemplateHaskell #-}

module SqliteSimpleTH where

import Language.Haskell.TH
import Control.Applicative
import Data.List
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField

deriveFromToRow :: Name -> Q [Dec]
deriveFromToRow name = (++) <$> (deriveFromRow name) <*> (deriveToRow name)

deriveFromRow :: Name -> Q [Dec]
deriveFromRow name = do
	TyConI (DataD _ _ _ [RecC contName fields] _) <- reify name
	let constraints = return []
	let boundNames = map (\n -> mkName $ "field" ++ show n) $ [1..length fields]
	let stmts = map (\localName -> makeBind localName) boundNames
	let final = [| return |] `appE` (foldl' appE (conE contName) (map varE boundNames))
	sequence
		[instanceD constraints (conT ''FromRow `appT` (conT name))
			[funD 'fromRow
				[
					clause []
					(normalB $ doE (stmts ++ [noBindS final]))
					[]
				]
			]
		]
	where
		makeBind :: Name -> StmtQ
		makeBind localName = do
			fieldF <- [| field |]
			return $ BindS (VarP localName) fieldF

deriveToRow :: Name -> Q [Dec]
deriveToRow name = do
	TyConI (DataD _ _ _ [RecC contName fields] _) <- reify name
	let constraints = return []
	let rowParam = mkName "r"
	let toFieldExps = map (\(fname, _, _) ->
		[|toField|] `appE` ((varE fname) `appE` (varE rowParam))) fields
	sequence
		[instanceD constraints (conT ''ToRow `appT` (conT name))
			[funD 'toRow
				[
					clause [varP rowParam]
					(normalB $ listE $ toFieldExps)
					[]
				]
			]
		]
