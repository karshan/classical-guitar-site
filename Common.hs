{-# LANGUAGE OverloadedStrings #-}
module Common where

import Data.String

serverBaseUrl :: (Monoid a, IsString a) => a
serverBaseUrl = "https://classicalguitarcalendar.com/"


