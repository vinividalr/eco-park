{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Tirolesas where

import Import
-- import Network.HTTP.Types.Status
-- import Database.Persist.Postgresql

getTiroR :: Handler Html
getTiroR = do
    guia <- lookupSession "_ID"
    tirolesas <- runDB $ selectList [] [Asc TirolesaNome]
    defaultLayout $ do
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
        $(whamletFile "templates/tirolesas.hamlet")

formTirolesa :: Maybe Tirolesa -> Form Tirolesa
formTirolesa mt = renderDivs $ Tirolesa
    <$> areq textField "Nome: " (fmap tirolesaNome mt)
    <*> areq textField "Distância: " (fmap tirolesaDistancia mt)
    <*> areq textField "Preço: " (fmap tirolesaPreco mt)

getCtiroR :: Handler Html
getCtiroR = do
    guia <- lookupSession "_ID"
    (widget,_) <- generateFormPost (formTirolesa Nothing)
    msg <- getMessage
    defaultLayout $ do
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
            table{
                width: 100%;
                display: flex;
                flex-direction: row;
                justify-content: center;
                align-items: center;
                border-bottom: 1px solid #D0CFCF

            }
            td{
                width: 100%;
                display: flex;
                flex-direction: row;
                justify-content: center;
                align-items: center;
                border-bottom: 1px solid #D0CFCF

            }

            th{
                width: 100%;
                display: flex;
                flex-direction: row;
                justify-content: center;
                align-items: center;
                border-bottom: 1px solid #D0CFCF

            }  
        |]

        [whamlet|
            <div class="topnav">
                <a href="/">
                    HOME
                $maybe email <- guia
                    <div class="topnav">
                        <a href="/tirolesas">
                            TIROLESAS
                        <a class="active" href="/criatirolesas">
                            CRIAR TIROLESAS    
                        <a href="/passeioagua">
                            RAFTING
                        <a href="/criarafting">
                            CRIAR RAFTING
                        <a href="/guia">
                            CRIAR GUIA
                        <a>
                            <form action=@{SairR} method=post>
                                #{email}: <input type="submit" value="LOGOUT">
                $nothing
                    <div class="topnav">
                        <a href="/guia">
                            CADASTRE-SE
                        <a href="/login">
                            LOGIN
                    
            $maybe mensa <- msg 
                <div>
                    ^{mensa}

            <h1>
                CRIAR UMA TIROLESA!!

            <form method=post action=@{CtiroR}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]
postCtiroR :: Handler Html
postCtiroR = do
    ((result,_),_) <- runFormPost (formTirolesa Nothing)
    case result of 
        FormSuccess tirolesa -> do 
            runDB $ insert tirolesa 
            setMessage [shamlet|
                <div>
                    TIROLESA INCLUIDA COM SUCESSO!!
            |]
            redirect CtiroR
        _ -> redirect HomeR

postApagarTirR :: TirolesaId -> Handler Html
postApagarTirR tid = do
    runDB $ delete tid 
    redirect TiroR

getEditarTirR :: TirolesaId -> Handler Html
getEditarTirR tid = do
    tirolesa <- runDB $ get404 tid
    (widget,_) <- generateFormPost (formTirolesa (Just tirolesa))
    msg <- getMessage
    defaultLayout $ do
        [whamlet|
            <form method=post action=@{EditarTirR tid}>
                ^{widget}
                <input type="submit" value="Editar">
        |]


postEditarTirR :: TirolesaId -> Handler Html
postEditarTirR tid = do
    _ <- runDB $ get404 tid
    ((result,_),_) <- runFormPost (formTirolesa Nothing)
    case result of
        FormSuccess novoTirolesa -> do
            runDB $ replace tid novoTirolesa
            redirect TiroR
        _ -> redirect HomeR    