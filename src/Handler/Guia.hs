{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Guia where

import Import

formLogin :: Form (Guia, Text)
formLogin = renderDivs $ (,)
    <$>(Guia
        <$> areq textField "E-mail: " Nothing
        <*> areq passwordField "Senha: " Nothing
    )
    <*> areq passwordField "Confirmação: " Nothing


getGuiaR :: Handler Html
getGuiaR = do
    guia <- lookupSession "_ID"
    (widget,_) <- generateFormPost formLogin
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
                        <a href="/criarafting">
                            CRIAR RAFTING
                        <a class="active" href="/guia">
                            CRIAR GUIA
                        <a>
                            <form action=@{SairR} method=post>
                                #{email}: <input type="submit" value="LOGOUT">

                $nothing
                    <div class="topnav">
                        <a class="active" href="/guia">
                            CADASTRE-SE
                        <a href="/login">
                            LOGIN
                    
            $maybe mensa <- msg 
                <div>
                    ^{mensa}

            <h1>
                CRIAR UM GUIA!!

            <form method=post action=@{GuiaR}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]
postGuiaR :: Handler Html
postGuiaR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess (guia@(Guia email senha), conf) -> do
            guiaExiste <- runDB $ getBy (UniqueEmail email)
            case guiaExiste of
                 Just _ -> do
                     setMessage [shamlet|
                            <div>
                                E-MAIL JA CADASTRADO!
                        |]
                     redirect GuiaR
                 Nothing -> do
                    if senha == conf then do
                        runDB $ insert guia
                        setMessage [shamlet|
                            <div>
                                GUIA INSERIDO COM SUCESSO!
                        |]
                        redirect GuiaR
                    else do
                        setMessage [shamlet|
                            <div>
                                SENHA E CONFIRMACAO DIFERENTES!
                        |]
                        redirect GuiaR
        _ -> redirect HomeR

