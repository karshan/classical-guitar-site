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
import           Data.List                   (find)
import           Data.Aeson                  (FromJSON, ToJSON)

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

data Festival = Festival {
    _ownerEmail :: String
  , _festivalName :: String -- for URL
  , _rawJSON :: String
} deriving (Eq, Show)

data Database = Database {
    _users :: [User]
  , _festivals :: [Festival]
}

makeLenses ''PasswordHash
makeLenses ''User
makeLenses ''Festival
makeLenses ''Database

$(deriveSafeCopy 0 'base ''Database)
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''PasswordHash)
$(deriveSafeCopy 0 'base ''Festival)

getUsers_ :: Query Database [User]
getUsers_ = view users <$> ask

-- returns false if user with same email already exists
addUser_ :: User -> Update Database Bool
addUser_ user = do
    db <- get
    let mExistingUser = find ((_email user ==) . _email) (db ^. users)
    maybe (put ((users %~ (user:)) db) >> return True)
        (const $ return False)
        mExistingUser

getFestivals_ :: Query Database [Festival]
getFestivals_ = view festivals <$> ask

-- returns false if festival with same name already exists
addFestival_ :: Festival -> Update Database Bool
addFestival_ festival = do
    db <- get
    let mExistingFestival = find ((_festivalName festival ==) . _festivalName) (db ^. festivals)
    maybe (put ((festivals %~ (festival:)) db) >> return True)
        (const $ return False)
        mExistingFestival

$(makeAcidic ''Database [ 'getUsers_
                        , 'addUser_
                        , 'getFestivals_
                        , 'addFestival_ ])

newtype DB a = DB { unDB :: ReaderT (AcidState Database) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (AcidState Database))

runDB :: DBContext -> DB a -> IO a
runDB db (DB a) = runReaderT a db

openDB :: FilePath -> IO DBContext
openDB fp = openLocalStateFrom fp (Database [] [])

update' a = liftIO . update a
query' a = liftIO . query a

--TODO generate with TemplateHaskell
addUser :: User -> DB Bool
addUser user = (`update'` AddUser_ user) =<< ask

getUsers :: DB [User]
getUsers = (`query'` GetUsers_) =<< ask

addFestival :: Festival -> DB Bool
addFestival festival = (`update'` AddFestival_ festival) =<< ask

getFestivals :: DB [Festival]
getFestivals = (`query'` GetFestivals_) =<< ask
