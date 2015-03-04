module Bootstrap where

import Text.Blaze.Internal
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text                   as T

data BootstrapButtonType = BtnDefault | BtnPrimary
                         | BtnSuccess | BtnInfo
                         | BtnWarning | BtnDanger
                         | BtnLink

bootstrapInput = H.input H.! A.class_ "form-control"

bootstrapButton BtnDefault = H.a H.! A.class_ "btn btn-default"
bootstrapButton BtnPrimary = H.a H.! A.class_ "btn btn-primary"
bootstrapButton BtnSuccess = H.a H.! A.class_ "btn btn-success"
bootstrapButton BtnInfo    = H.a H.! A.class_ "btn btn-info"
bootstrapButton BtnWarning = H.a H.! A.class_ "btn btn-warning"
bootstrapButton BtnDanger  = H.a H.! A.class_ "btn btn-danger"
bootstrapButton BtnLink    = H.a H.! A.class_ "btn btn-link"

bootstrapRow f = H.div H.! A.class_ "row" $ f

bootstrapColumn n | n < 1  = error "Too few columns!"
bootstrapColumn n | n > 12 = error "Too many columns!"
bootstrapColumn 1 = H.div H.! A.class_ "col-md-1"
bootstrapColumn 2 = H.div H.! A.class_ "col-md-2"
bootstrapColumn 3 = H.div H.! A.class_ "col-md-3"
bootstrapColumn 4 = H.div H.! A.class_ "col-md-4"
bootstrapColumn 5 = H.div H.! A.class_ "col-md-5"
bootstrapColumn 6 = H.div H.! A.class_ "col-md-6"
bootstrapColumn 7 = H.div H.! A.class_ "col-md-7"
bootstrapColumn 8 = H.div H.! A.class_ "col-md-8"
bootstrapColumn 9 = H.div H.! A.class_ "col-md-8"
bootstrapColumn 10 = H.div H.! A.class_ "col-md-8"
bootstrapColumn 11 = H.div H.! A.class_ "col-md-8"
bootstrapColumn 12 = H.div H.! A.class_ "col-md-8"

data BootstrapFormType = FormVertical | FormInline | FormHorizontal
bootstrapForm FormVertical   = H.form
bootstrapForm FormInline     = H.form H.! A.class_ "form-inline"
bootstrapForm FormHorizontal = H.form H.! A.class_ "form-horizontal"

bootstrapFormGroup = H.div H.! A.class_ "form-group"

