{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Rafting where

import Import
-- import Network.HTTP.Types.Status
-- import Database.Persist.Postgresql

getPaguaR :: Handler Html
getPaguaR = do
    guia <- lookupSession "_ID"
    raftings <- runDB $ selectList [] [Asc RaftingNome]
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
        $(whamletFile "templates/passeiosagua.hamlet")

formRafting :: Maybe Rafting -> Form Rafting
formRafting mr = renderDivs $ Rafting
    <$> areq textField "Nome: " (fmap raftingNome mr)
    <*> areq textField "Rio: " (fmap raftingRio mr)
    <*> areq textField "Preço: " (fmap raftingPreco mr)

getCraftingR :: Handler Html
getCraftingR = do
    guia <- lookupSession "_ID"
    (widget,_) <- generateFormPost (formRafting Nothing)
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
            botao{
                margin-top: 5px;
                border: 1px solid #464646;
                border-radius: 5px;

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
                        <a href="/criatirolesas">
                            CRIAR TIROLESAS    
                        <a href="/passeioagua">
                            RAFTING
                        <a class="active" href="/criarafting">
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
                CRIAR UM PASSEIO RAFTING!

            <div class="formulario">   
                <form method=post action=@{CraftingR}>
                    ^{widget}
                    <input class="botao" type="submit" value="Cadastrar">
        |]
postCraftingR :: Handler Html
postCraftingR = do
    ((result,_),_) <- runFormPost (formRafting Nothing)
    case result of 
        FormSuccess rafting -> do 
            runDB $ insert rafting 
            setMessage [shamlet|
                <div>
                    PASSEIO RAFTING INCLUIDO COM SUCESSO!!
            |]
            redirect CraftingR
        _ -> redirect HomeR

postApagarRafR :: RaftingId -> Handler Html
postApagarRafR rid = do
    runDB $ delete rid 
    redirect PaguaR 


getEditarRafR :: RaftingId -> Handler Html
getEditarRafR rid = do
    rafting <- runDB $ get404 rid
    (widget,_) <- generateFormPost (formRafting (Just rafting))
    msg <- getMessage
    defaultLayout $ do
        [whamlet|
            <form method=post action=@{EditarRafR rid}>
                ^{widget}
                <input type="submit" value="Editar">
        |]

postEditarRafR :: RaftingId -> Handler Html
postEditarRafR rid = do
    _ <- runDB $ get404 rid
    ((result,_),_) <- runFormPost (formRafting Nothing)
    case result of
        FormSuccess novoRafting -> do
            runDB $ replace rid novoRafting
            redirect PaguaR
        _ -> redirect HomeR    