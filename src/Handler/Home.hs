{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
-- import Network.HTTP.Types.Status
-- import Database.Persist.Postgresql

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        guia <- lookupSession "_ID"
        toWidgetHead [lucius|
            html{
                background: url(static/img/aventura.png);
                background-size: auto;
                background-repeat: no-repeat;
                background-position: center;
                background-attachment: fixed;
            }
            h1{
                text-align: left;
                color: #556B2F;
            }            
            .topnav {
              background-color: #333;
              overflow: hidden;
            }
            .topnav a {
                float: left;
                color: #f2f2f2;
                text-align: center;
                padding: 14px 16px;
                text-decoration: none;
                font-size: 17px;
            }
            .topnav a:hover {
                background-color: #ddd;
                color: black;
            }
            .topnav a.active {
              background-color: #556B2F;
              color: white;
            }  
        |]
        $(whamletFile "templates/home.hamlet")

