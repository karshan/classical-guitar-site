{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-unused-binds  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module DB where

import           Control.Lens                (view, (%~), (&), (.~), (^.))
import           Control.Lens.TH             (makeLenses)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.State         (get, put)
import           Data.Acid                   (AcidState, Query, Update, makeAcidic,
                                              openLocalStateFrom, query, update)
import           Data.ByteString             (ByteString)
import           Data.SafeCopy               (base, deriveSafeCopy)

type DBContext = AcidState Database

data PasswordHash = PasswordHash {
    _salt :: ByteString
  , _hash :: ByteString
} deriving (Eq, Show)

data User = User {
    _firstName :: String
  , _lastName :: String
  , _email :: String
  , _passwordHash :: PasswordHash
} deriving (Eq, Show)

data Database = Database {
    _users :: [User]
}

makeLenses ''PasswordHash
makeLenses ''User
makeLenses ''Database

$(deriveSafeCopy 0 'base ''Database)
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''PasswordHash)

getUsers_ :: Query Database [User]
getUsers_ = view users <$> ask

addUser_ :: User -> Update Database ()
addUser_ user = put . (users %~ (user:)) =<< get

$(makeAcidic ''Database [ 'getUsers_, 'addUser_ ])

newtype DB a = DB { unDB :: ReaderT (AcidState Database) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (AcidState Database))

runDB :: DBContext -> DB a -> IO a
runDB db (DB a) = runReaderT a db

openDB :: FilePath -> IO DBContext
openDB fp = openLocalStateFrom fp (Database [])

update' a = liftIO . update a
query' a = liftIO . query a

--TODO generate with TemplateHaskell
addUser :: User -> DB ()
addUser user = (`update'` AddUser_ user) =<< ask

getUsers :: DB [User]
getUsers = (`query'` GetUsers_) =<< ask
