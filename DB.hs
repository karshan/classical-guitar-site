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

import           Control.Lens           (view, (%~), (&), (.~), (^.))
import           Control.Lens.TH        (makeLenses)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.State    (get, put)
import           Data.Acid              (AcidState, Query, Update, makeAcidic,
                                         openLocalStateFrom, query, update)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.ByteString        (ByteString)
import           Data.Foldable          (find)
import           Data.SafeCopy          (base, deriveSafeCopy)
import           Data.Set

type DBContext = AcidState Database

data PasswordHash = PasswordHash {
    _salt :: ByteString
  , _hash :: ByteString
} deriving (Eq, Show, Ord)

data User = User {
    _firstName    :: String
  , _lastName     :: String
  , _email        :: String -- Primary Key
  , _passwordHash :: PasswordHash
  , _activated    :: Bool
} deriving (Eq, Show, Ord)

data Festival = Festival {
    _ownerEmail   :: String
  , _URISafeName :: String -- for URL so Primary Key
  , _rawJSON      :: String
} deriving (Eq, Show, Ord)

data ContactForm = ContactForm {
    _contactName  :: String
  , _contactEmail :: String
  , _msgSubject   :: String
  , _msgBody      :: String
} deriving (Show)

data Database = Database {
    _users     :: Set User
  , _festivals :: Set Festival
}

makeLenses ''PasswordHash
makeLenses ''User
makeLenses ''Festival
makeLenses ''Database

$(deriveSafeCopy 0 'base ''Database)
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''PasswordHash)
$(deriveSafeCopy 0 'base ''Festival)

getUsers_ :: Query Database (Set User)
getUsers_ = view users <$> ask

-- If a user with the same email already exists the user is not added and the existing user is returned
addUser_ :: User -> Update Database (Maybe User)
addUser_ user = do
    db <- get
    let mExistingUser = find ((_email user ==) . _email) (db ^. users)
    maybe (put ((users %~ (insert user)) db) >> return Nothing)
        (return . Just)
        mExistingUser

-- returns Nothing if the user with specified email wasn't found
activateUser_ :: String -> Update Database (Maybe User)
activateUser_ email = do
    db <- get
    let mUser = find ((email ==) . _email) (db ^. users)
    maybe (return Nothing)
        (\user -> do
            let activatedUser = user & activated .~ True
            put ((users %~ (insert activatedUser . delete user)) db) >> return (Just activatedUser))
        mUser

getFestivals_ :: Query Database (Set Festival)
getFestivals_ = view festivals <$> ask

-- returns false if festival with same name already exists
addFestival_ :: Festival -> Update Database Bool
addFestival_ festival = do
    db <- get
    let mExistingFestival = find ((_URISafeName festival ==) . _URISafeName) (db ^. festivals)
    maybe (put ((festivals %~ (insert festival)) db) >> return True)
        (const $ return False)
        mExistingFestival

$(makeAcidic ''Database [ 'getUsers_
                        , 'addUser_
                        , 'activateUser_
                        , 'getFestivals_
                        , 'addFestival_ ])

newtype DB a = DB { unDB :: ReaderT (AcidState Database) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (AcidState Database))

runDB :: DBContext -> DB a -> IO a
runDB db (DB a) = runReaderT a db

openDB :: FilePath -> IO DBContext
openDB fp = openLocalStateFrom fp (Database empty empty)

update' a = liftIO . update a
query' a = liftIO . query a

--TODO generate with TemplateHaskell
addUser :: User -> DB (Maybe User)
addUser user = (`update'` AddUser_ user) =<< ask

activateUser :: String -> DB (Maybe User)
activateUser email = (`update'` ActivateUser_ email) =<< ask

getUsers :: DB (Set User)
getUsers = (`query'` GetUsers_) =<< ask

addFestival :: Festival -> DB Bool
addFestival festival = (`update'` AddFestival_ festival) =<< ask

getFestivals :: DB (Set Festival)
getFestivals = (`query'` GetFestivals_) =<< ask
