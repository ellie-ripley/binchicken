{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Foundation where

import Data.Kind (Type)

import Import.NoFoundation
    ( snd,
      ($),
      Eq((==)),
      Monad(return),
      Bool(..),
      Maybe(..),
      IO,
      Either,
      (<$>),
      flip,
      (||),
      Text,
      ReaderT,
      SqlBackend,
      isJust,
      isNothing,
      otherwise,
      (++),
      defaultClientSessionBackend,
      defaultYesodMiddleware,
      getApprootText,
      guessApproot,
      widgetToPageContent,
      defaultCsrfCookieName,
      defaultCsrfHeaderName,
      getCurrentRoute,
      getMessage,
      getYesod,
      withUrlRenderer,
      mkYesodData,
      addStylesheetRemoteAttrs,
      parseRoutesFile,
      defaultFormMessage,
      defaultGetDBRunner,
      base64md5,
      (.),
      fmap,
      join,
      (=.),
      unpack,
      liftIO,
      get,
      putStrLn,
      LByteString,
      Html,
      HasHttpManager(..),
      Manager,
      LogLevel(LevelError, LevelWarn),
      Entity(Entity),
      Unique(UniqueUser),
      PersistUniqueRead(getBy),
      SqlPersistT,
      Lang,
      RenderMessage(..),
      MonadUnliftIO,
      YesodBreadcrumbs(..),
      MonadHandler(liftHandler, HandlerSite),
      Yesod(approot, makeLogger, shouldLogIO, addStaticContent,
            isAuthorized, authRoute, defaultLayout, yesodMiddleware,
            makeSessionBackend),
      ToTypedContent,
      Approot(ApprootRequest),
      AuthResult(Authorized, Unauthorized),
      HandlerFor,
      PageContent(pageTitle, pageHead, pageBody),
      SessionBackend,
      RenderRoute(renderRoute, Route),
      Route(StaticRoute, LogoutR, LoginR),
      FormMessage,
      FormResult,
      MForm,
      DBRunner,
      YesodPersist(..),
      YesodPersistRunner(..),
      Static,
      AppSettings(appRoot, appCopyright, appAnalytics, appStaticDir,
                  appShouldLogAll),
      widgetFile,
      UserId,
      User(User, userEmail, userPassHash),
      img_binchicken_jpg,
      getAuth,
      maybeAuthPair,
      AuthPlugin,
      AuthenticationResult(UserError, Authenticated),
      Creds(credsIdent),
      YesodAuth(maybeAuthId, authPlugins, authenticate,
                redirectToReferer, logoutDest, loginDest, AuthId),
      YesodAuthPersist (getAuthEntity),
      Auth,
      EntityField(..),
      userVerkey,
      userPassHash)
import Database.Persist.Sql (ConnectionPool, insert, runSqlPool, update)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet          (hamletFile, shamlet)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)

import Network.Mail.Mime ( Address(..)
                         , Disposition(..)
                         , Encoding(..)
                         , Mail(..)
                         , Part(..)
                         , PartContent(..)
                         , emptyMail
                         )
import Network.Mail.Mime.SES
import Text.Shakespeare.Text (stext)
import Yesod.Auth.Email

import Yesod.Auth.Message   (AuthMessage(..))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TEL
import qualified Data.Password.Bcrypt as BC
import Network.Mail.Mime.SES (SES)


-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data BinChicken = BinChicken
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route BinChicken
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor BinChicken
-- type Widget = WidgetFor BinChicken ()
mkYesodData "BinChicken" $(parseRoutesFile "config/routes.yesodroutes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor BinChicken) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: Type -> Type).
    (MonadUnliftIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod BinChicken where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot BinChicken
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: BinChicken -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuthPair
        mcurrentRoute <- getCurrentRoute

        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = "Home"
                    , menuItemRoute = HomeR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Exercises"
                    , menuItemRoute = ExercisesR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "My Progress"
                    , menuItemRoute = ProgressR
                    , menuItemAccessCallback = isJust muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Register"
                    , menuItemRoute = RegisterR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Login"
                    , menuItemRoute = AuthR LoginR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Logout"
                    , menuItemRoute = AuthR LogoutR
                    , menuItemAccessCallback = isJust muser
                    }
                ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
          addStylesheetRemoteAttrs "https://cdn.jsdelivr.net/npm/bootstrap@5.1.1/dist/css/bootstrap.min.css"
                                   [ ("crossorigin", "anonymous")
                                   , ("integrity", "sha384-F3w7mX95PdgyTmZZMECAngseQB83DfGTowi0iMjiWaeVhAn4FJkqJByhZMI3AhiU")
                                   ]
          $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute
        :: BinChicken
        -> Maybe (Route BinChicken)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route BinChicken  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized RegisterR _ = return Authorized
    isAuthorized ExercisesR _ = return Authorized
    isAuthorized DummyExerciseR _ = return Authorized
    isAuthorized MainConnectiveR _ = return Authorized
    isAuthorized EvalBooleanR _ = return Authorized
    isAuthorized EvalStrongKleeneR _ = return Authorized
    isAuthorized EvalDunnBelnapR _ = return Authorized
    isAuthorized CounterexClassicalR _ = return Authorized
    isAuthorized CounterexNonclassicalR _ = return Authorized
    isAuthorized ProofRequirementsR _ = return Authorized
    isAuthorized ProofIntuitionisticR _ = return Authorized
    isAuthorized ProofNormalizeR _ = return Authorized
    isAuthorized ProofPlaygroundR _ = return Authorized
    isAuthorized CounterexIntuitionisticR _ = return Authorized

    -- the profile route requires that the user is authenticated, so we
    -- delegate to that function
    isAuthorized ProgressR _ = isAuthenticated
    isAuthorized ProfileR _ = isAuthenticated

    isAuthorized SummaryR _ = return (Unauthorized "Summary page currently disabled")
    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route BinChicken, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: BinChicken -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: BinChicken -> IO Logger
    makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs BinChicken where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    breadcrumb
        :: Route BinChicken  -- ^ The route the user is visiting currently.
        -> Handler (Text, Maybe (Route BinChicken))
    breadcrumb HomeR = return ("Home", Nothing)
    breadcrumb (AuthR _) = return ("Login", Just HomeR)
    breadcrumb ProfileR = return ("Profile", Just HomeR)
    breadcrumb  _ = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist BinChicken where
    type YesodPersistBackend BinChicken = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner BinChicken where
    getDBRunner :: Handler (DBRunner BinChicken, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

sesCreds :: SES
sesCreds = let x = x in x

instance YesodAuthEmail BinChicken where
    type AuthEmailId BinChicken = UserId

    afterPasswordRoute _ = HomeR

    addUnverified email verkey =
        liftHandler $ runDB $ insert $ User email Nothing (Just verkey) False

    sendVerifyEmail email _ verurl = do
        -- Print out to the console the verification email, for easier
        -- debugging.
        liftIO $ putStrLn $ "Copy/ Paste this URL in your browser: " ++ verurl

        -- Send email.
        liftIO $ renderSendMailSESGlobal sesCreds (emptyMail $ Address Nothing "noreply")
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", "Verify your email address")
                ]
            , mailParts = [[textPart, htmlPart]]
            }
      where
        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partDisposition = DefaultDisposition
            , partContent = PartContent $  TEL.encodeUtf8
                [stext|
                    Please confirm your email address by clicking on the link below.

                    #{verurl}

                    Thank you
                |]
            , partHeaders = []
            }
        htmlPart = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partDisposition = DefaultDisposition
            , partContent = PartContent . TEL.encodeUtf8 $ renderHtml
                [shamlet|
                    <p>Please confirm your email address by clicking on the link below.
                    <p>
                        <a href=#{verurl}>#{verurl}
                    <p>Thank you
                |]
            , partHeaders = []
            }
    getVerifyKey = liftHandler . runDB . fmap (join . fmap userVerkey) . get
    setVerifyKey uid key = liftHandler $ runDB $ update uid [UserVerkey =. Just key]
    verifyAccount uid = liftHandler $ runDB $ do
        mu <- get uid
        case mu of
            Nothing -> return Nothing
            Just u -> do
                update uid [UserVerified =. True, UserVerkey =. Nothing]
                return $ Just uid
    getPassword = liftHandler . runDB . fmap (join . fmap userPassHash) . get
    setPassword uid pass = liftHandler . runDB $ update uid [UserPassHash =. Just pass]
    getEmailCreds email = liftHandler $ runDB $ do
        mu <- getBy $ UniqueUser email
        case mu of
            Nothing -> return Nothing
            Just (Entity uid u) -> return $ Just EmailCreds
                { emailCredsId = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust $ userPassHash u
                , emailCredsVerkey = userVerkey u
                , emailCredsEmail = email
                }
    getEmail = liftHandler . runDB . fmap (fmap userEmail) . get



instance YesodAuth BinChicken where
    type AuthId BinChicken = UserId

    -- Where to send a user after successful login
    loginDest :: BinChicken -> Route BinChicken
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: BinChicken -> Route BinChicken
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: BinChicken -> Bool
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ BinChicken)
                 => Creds BinChicken -> m (AuthenticationResult BinChicken)
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> return . UserError . IdentifierNotFound $ ("No such user!" :: Text)

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: BinChicken -> [AuthPlugin BinChicken]
    authPlugins _ = [authEmail]

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

-- | Access function to determine if a user is an admin user
isAdmin :: Handler AuthResult
isAdmin = do
  let unauth = Unauthorized "You must be logged in as an admin to access this page"
  muid <- maybeAuthId
  case muid of
    Nothing  -> return unauth
    Just uid -> do
      muent <- getAuthEntity uid
      case muent of
        Nothing -> return unauth
        Just us
          | userEmail us == "davewripley@gmail.com" -> return Authorized
          | otherwise -> return unauth

instance YesodAuthPersist BinChicken



-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage BinChicken FormMessage where
    renderMessage :: BinChicken -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager BinChicken where
    getHttpManager :: BinChicken -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: BinChicken -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger


-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
