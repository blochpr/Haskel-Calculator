{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import           Yesod
import           Data.Text (Text, unpack)

data App = App

mkYesod "App" [parseRoutes|
/home		HomeR GET
/add/#Int/#Int AddR GET
/sub/#Int/#Int SubR GET
/mul/#Int/#Int MulR GET
/div/#Int/#Int DivR GET
|]

instance Yesod App

getHomeR :: Handler TypedContent
getHomeR = selectRep $ do
    provideRep $ defaultLayout $ do
        [whamlet|
<p>Simple Haskell RESTfull Calculator|]
 
 
getAddR :: Int -> Int -> Handler TypedContent
getAddR x y = selectRep $ do
    provideRep $ defaultLayout $ do
        [whamlet|
<p>#{x} + #{y} = #{z}|]
    provideJson $ object ["result" .= z]
  where
    z = x + y

getSubR :: Int -> Int -> Handler TypedContent
getSubR x y = selectRep $ do
    provideRep $ defaultLayout $ do
        [whamlet|
<p>#{x} - #{y} = #{z}|]
    provideJson $ object ["result" .= z]
  where
    z = x - y

getMulR :: Int -> Int -> Handler TypedContent
getMulR x y = selectRep $ do
    provideRep $ defaultLayout $ do
        [whamlet|
<p>#{x} * #{y} = #{z}|]
    provideJson $ object ["result" .= z]
  where
    z = x * y

getDivR :: Int -> Int -> Handler TypedContent
getDivR x y = selectRep $ do
    provideRep $ defaultLayout $ do
        [whamlet|
<p>#{x} / #{y} = #{z}|]
    provideJson $ object ["result" .= z]
  where
    z = divfloat x y

divfloat :: Int -> Int -> Double
divfloat x y = (fromIntegral x) / (fromIntegral y)


main :: IO ()
main = warp 3000 App
