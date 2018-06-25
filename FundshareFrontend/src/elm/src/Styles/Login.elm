module Styles.Login
    exposing
        ( LoginStyleClass(..)
        , c
        , cls
        , cs
        , loginStylesheet
        )

import Html exposing (..)
import Material.Options as Options
import Styles.Internal exposing (..)


c =
    Styles.Internal.c


type LoginStyleClass
    = Form
    | BtnLogin


cls : LoginStyleClass -> String
cls c =
    c |> t2k


cs : LoginStyleClass -> Options.Property c m
cs c =
    cls c |> Options.cs


loginStylesheet : String
loginStylesheet =
    c Form ++ """{
        display: inline-flex;
        flex-direction: column;
        padding: 25px;
        position: relative;
    }
    """ ++ c Form ++ """ > .mdl-progress {
        position: absolute;
        bottom: 0;
        left: 0;
        right: 0;
    }
    """ ++ c BtnLogin ++ """{
        margin-top: 20px;
        align-self: flex-start;
    }"""
