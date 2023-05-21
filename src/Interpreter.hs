{-# LANGUAGE TupleSections #-}

module Interpreter (interpret, InterpreterError (..)) where

import Codec.Picture
import Codec.Picture.Types
import Control.Exception (SomeException, catch)
import Control.Monad (forM_)
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor (first)
import Data.Either (rights)
import Data.List (intercalate, isSuffixOf)
import Data.Map (Map)
import Debug.Trace
import Graphics.Image (Pixel (PixelRGB), VU (..), makeImageR)
import Graphics.Image.IO (fromJPImageRGB8, fromJPImageRGBA8, toJPImageRGB8, toJPImageRGBA8)
import Graphics.Image.Processing (Bicubic (..), Bilinear (..), Border (..), Interpolation (..), Nearest (..), flipH, flipV, interpolate, leftToRight, resize, rotate, rotate180, scale)
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Text.TrueType (Font, loadFontFile)
import Parser (Expression (..), Statement (..))
import System.Directory
import System.FilePath

type Value = Expression

type Output = [String]

data Environment = Environment
  { envBindings :: [(String, Expression)],
    envOutput :: [String]
  }
  deriving (Show)

emptyEnvironment :: Environment
emptyEnvironment = Environment [] []

data InterpreterError
  = UnboundVariable String
  | BadFunction String
  | BadArgument String
  | BadMethod String
  deriving (Show, Eq)

type InterpreterM a = ExceptT InterpreterError (StateT Environment IO) a

interpret :: [Statement] -> IO (Either InterpreterError (), [String])
interpret stmts = do
  let initialState = emptyEnvironment
      computation = mapM_ interpretStatement stmts
  (result, finalState) <- runStateT (runExceptT computation) initialState
  return (result, envOutput finalState)

interpretStatements :: [Statement] -> InterpreterM ()
interpretStatements = mapM_ interpretStatement

interpretStatement :: Statement -> InterpreterM ()
interpretStatement stmt = case stmt of
  Assignment ident expr -> do
    value <- interpretExpression expr
    modify (\env -> env {envBindings = (ident, value) : envBindings env})
  MethodCallStmt expr -> do
    (obj, method, args) <- methodCallToParts expr
    result <- interpretExpression expr
    case result of
      ImageExpression imgPath img -> modify (\env -> env {envBindings = (obj, result) : envBindings env})
      _ -> return ()
  FunctionCallStmt expr -> void $ interpretExpression expr
  ForLoop var expr body -> do
    list <- interpretExpression expr
    case list of
      ListExpression elements -> forM_ elements $ \element -> do
        modify (\env -> env {envBindings = (var, element) : envBindings env})
        interpretStatements body
      _ -> throwError (BadArgument "Expected a list expression")

methodCallToParts :: Expression -> InterpreterM (String, Expression, [Expression])
methodCallToParts (MethodCall (Identifier ident) methodName args) = return (ident, Identifier methodName, args)
methodCallToParts _ = throwError (BadArgument "Expected a valid method call expression")

interpretExpression :: Expression -> InterpreterM Expression
interpretExpression expr = case expr of
  StringLit s -> return $ StringLit s
  NumberLit n -> return $ NumberLit n
  Identifier ident -> do
    env <- get
    case lookup ident (envBindings env) of
      Nothing -> throwError (UnboundVariable ident)
      Just v -> return v
  FunctionCall name args -> do
    argValues <- mapM interpretExpression args
    applyFunction name argValues
  ListExpression elements -> do
    newElements <- mapM interpretExpression elements
    return $ ListExpression newElements
  MethodCall obj method args -> do
    objValue <- interpretExpression obj
    argValues <- mapM interpretExpression args
    applyMethod objValue method argValues
  Add left right -> do
    leftValue <- interpretExpression left
    rightValue <- interpretExpression right
    addValues leftValue rightValue
  TupleExpression elements -> do
    newElements <- mapM interpretExpression elements
    return $ TupleExpression newElements

addValues :: Expression -> Expression -> InterpreterM Expression
addValues (NumberLit n1) (NumberLit n2) = return $ NumberLit (n1 + n2)
addValues (StringLit s1) (StringLit s2) = return $ StringLit (s1 ++ s2)
addValues (ImageExpression _ img1) (ImageExpression _ img2) = do
  let combinedImg = leftToRight (fromJPImageRGB8 (convertRGB8 img1)) (fromJPImageRGB8 (convertRGB8 img2))
  return $ ImageExpression Nothing (ImageRGB8 (toJPImageRGB8 combinedImg))
addValues _ _ = throwError $ BadArgument "Invalid argument types for addition"

applyFunction :: String -> [Expression] -> InterpreterM Expression
applyFunction "print" args = do
  let output = unwords (map exprToString args)
  modify (\env -> env {envOutput = envOutput env ++ [output], envBindings = envBindings env})
  return $ StringLit ""
applyFunction "load" [StringLit path] = do
  result <- liftIO $ readImage path
  case result of
    Left err -> throwError $ BadArgument ("Error loading image: " ++ err)
    Right img -> return $ ImageExpression (Just path) img
applyFunction "load" _ = throwError $ BadArgument "load function expects a single string argument"
applyFunction "loadFolder" [StringLit path] = do
  images <- liftIO $ loadFolderUnchecked path
  return $ ListExpression images
applyFunction "loadFolder" _ = throwError $ BadArgument "loadFolder function expects a single string argument"
applyFunction "generate" [NumberLit x, NumberLit y, TupleExpression [NumberLit r, NumberLit g, NumberLit b, NumberLit a]] = do
  let clamp val = round $ max 0 (min val 255)
      img = generateImg x y (PixelRGBA8 (clamp $ fromIntegral r) (clamp $ fromIntegral g) (clamp $ fromIntegral b) (clamp $ fromIntegral a))
  return $ ImageExpression Nothing (ImageRGBA8 img)
applyFunction name _ = throwError $ BadFunction name

applyMethod :: Expression -> String -> [Expression] -> InterpreterM Expression
applyMethod (ImageExpression imgPath img) "save" [StringLit path]
  | ".png" `isSuffixOf` path = do
      liftIO $ savePngImage path img
      return $ ImageExpression imgPath img
  | ".jpg" `isSuffixOf` path = do
      liftIO $ saveJpgImage 100 path img
      return $ ImageExpression imgPath img
  | ".jpeg" `isSuffixOf` path = do
      liftIO $ saveJpgImage 100 path img
      return $ ImageExpression imgPath img
  | otherwise = throwError $ BadArgument "Unsupported image format, only .png, .jpg and .jpeg are supported"
applyMethod (ImageExpression imgPath img) "sepiaFilter" [] = do
  let sepiaImg = sepiaFilter (convertRGBA8 img)
  return $ ImageExpression imgPath (ImageRGBA8 sepiaImg)
applyMethod (ImageExpression imgPath img) "grayscale" [] = do
  let bwImg = grayscaleFilter (convertRGBA8 img)
  return $ ImageExpression imgPath (ImageRGBA8 bwImg)
applyMethod (ImageExpression (Just path) _) "fileName" [] = return $ StringLit path
applyMethod (ImageExpression Nothing _) "fileName" [] = throwError $ BadArgument "Image does not have an associated file name"
applyMethod (ImageExpression imgPath img) "text" [StringLit text, NumberLit fontSize, NumberLit x, NumberLit y, StringLit fontFile] = do
  let imgRGBA8 = convertRGBA8 img
  font <- liftIO $ loadFontFile fontFile
  case font of
    Left err -> throwError $ BadArgument "Font file does not work"
    Right font -> do
      let clamp val = round $ max 0 (min val 255)
          position = V2 (fromIntegral (imageWidth imgRGBA8) * (fromIntegral x / 100)) (fromIntegral (imageHeight imgRGBA8) * (fromIntegral y / 100))
          outputImage = addTextToImage imgRGBA8 font (fromIntegral fontSize) position text (PixelRGB8 (clamp 0) (clamp 0) (clamp 0))
      return $ ImageExpression imgPath (ImageRGBA8 outputImage)
applyMethod (ImageExpression imgPath img) "text" [StringLit text, NumberLit fontSize, NumberLit x, NumberLit y, StringLit fontFile, TupleExpression [NumberLit r, NumberLit g, NumberLit b]] = do
  let imgRGBA8 = convertRGBA8 img
  font <- liftIO $ loadFontFile fontFile
  case font of
    Left err -> throwError $ BadArgument "Font file does not work"
    Right font -> do
      let clamp val = round $ max 0 (min val 255)
          position = V2 (fromIntegral (imageWidth imgRGBA8) * (fromIntegral x / 100)) (fromIntegral (imageHeight imgRGBA8) * (fromIntegral y / 100))
          outputImage = addTextToImage imgRGBA8 font (fromIntegral fontSize) position text (PixelRGB8 (clamp $ fromIntegral r) (clamp $ fromIntegral g) (clamp $ fromIntegral b))
      return $ ImageExpression imgPath (ImageRGBA8 outputImage)
applyMethod (ImageExpression imgPath img) "rotate" [NumberLit degrees] = do
  let rotatedImage = rotate Nearest (Fill 0) (fromIntegral degrees * (pi / 180)) (fromJPImageRGBA8 (convertRGBA8 img))
  return $ ImageExpression imgPath (ImageRGBA8 (toJPImageRGBA8 rotatedImage))
applyMethod (ImageExpression imgPath img) "flipV" [] = do
  let flippedImage = flipV (fromJPImageRGBA8 (convertRGBA8 img))
  return $ ImageExpression imgPath (ImageRGBA8 (toJPImageRGBA8 flippedImage))
applyMethod (ImageExpression imgPath img) "flipH" [] = do
  let flippedImage = flipH (fromJPImageRGBA8 (convertRGBA8 img))
  return $ ImageExpression imgPath (ImageRGBA8 (toJPImageRGBA8 flippedImage))
applyMethod (ImageExpression imgPath img1) "overlay" [ImageExpression _ img2, NumberLit x, NumberLit y] = do
  let base = convertRGBA8 img1
      overlay = convertRGBA8 img2
      position = V2 (fromIntegral (imageWidth base) * (fromIntegral x / 100)) (fromIntegral (imageHeight base) * (fromIntegral y / 100))
      resultImage = overlayImages base overlay position
  return $ ImageExpression imgPath (ImageRGBA8 resultImage)
applyMethod (ImageExpression imgPath img) "scale" [NumberLit x, NumberLit y] = do
  let scaledImage = scale Nearest Edge (fromIntegral y / 100, fromIntegral x / 100) (fromJPImageRGBA8 (convertRGBA8 img))
  return $ ImageExpression imgPath (ImageRGBA8 (toJPImageRGBA8 scaledImage))
applyMethod (ImageExpression imgPath img) "resize" [NumberLit x, NumberLit y] = do
  let resizedImage = resize Nearest Edge (fromIntegral y, fromIntegral x) (fromJPImageRGBA8 (convertRGBA8 img))
  return $ ImageExpression imgPath (ImageRGBA8 (toJPImageRGBA8 resizedImage))
applyMethod (ImageExpression imgPath img) "width" [] = do
  let imgWidth = imageWidth $ convertRGBA8 img
  return $ NumberLit imgWidth
applyMethod (ImageExpression imgPath img) "height" [] = do
  let imgHeight = imageHeight $ convertRGBA8 img
  return $ NumberLit imgHeight
applyMethod (ImageExpression imgPath img) "dimensions" [] = do
  let imgWidth = imageWidth $ convertRGBA8 img
      imgHeight = imageHeight $ convertRGBA8 img
  return $ TupleExpression [NumberLit imgWidth, NumberLit imgHeight]
applyMethod obj methodName _ = throwError $ BadMethod $ methodName ++ " (object: " ++ show obj ++ ")"

sepiaFilter :: Image PixelRGBA8 -> Image PixelRGBA8
sepiaFilter img =
  let sepiaPixel :: PixelRGBA8 -> PixelRGBA8
      sepiaPixel (PixelRGBA8 r g b a) =
        let tr = 0.393 * fromIntegral r + 0.769 * fromIntegral g + 0.189 * fromIntegral b
            tg = 0.349 * fromIntegral r + 0.686 * fromIntegral g + 0.168 * fromIntegral b
            tb = 0.272 * fromIntegral r + 0.534 * fromIntegral g + 0.131 * fromIntegral b
            clamp val = round $ max 0 (min val 255)
         in PixelRGBA8 (clamp tr) (clamp tg) (clamp tb) a
   in pixelMap sepiaPixel img

grayscaleFilter :: Image PixelRGBA8 -> Image PixelRGBA8
grayscaleFilter img =
  let grayPixel :: PixelRGBA8 -> PixelRGBA8
      grayPixel (PixelRGBA8 r g b a) =
        let gray = 0.3 * fromIntegral r + 0.59 * fromIntegral g + 0.1 * fromIntegral b
            clamp val = round $ max 0 (min val 255)
         in PixelRGBA8 (clamp gray) (clamp gray) (clamp gray) a
   in pixelMap grayPixel img

exprToString :: Expression -> String
exprToString (StringLit s) = s
exprToString (NumberLit n) = show n
exprToString expr = show expr

loadFolderUnchecked :: FilePath -> IO [Expression]
loadFolderUnchecked folderPath = do
  images <- loadFolder folderPath
  return $ map (uncurry ImageExpression) (rights images)

loadFolder :: FilePath -> IO [Either (Maybe FilePath, String) (Maybe FilePath, DynamicImage)]
loadFolder folderPath = do
  files <- listDirectory folderPath
  let imageFiles = filter isImage files
  mapM (\f -> (first (Just f,) . fmap (Just f,) <$> readImage (folderPath </> f)) `catch` handleErr f) imageFiles
  where
    isImage f = any (`isSuffixOf` f) [".png", ".jpg", ".jpeg"]
    handleErr f e = return $ Left (Just f, show (e :: SomeException))

drawText :: Font -> Float -> V2 Float -> String -> PixelRGB8 -> Drawing PixelRGBA8 ()
drawText font fontSize position text (PixelRGB8 r g b) = do
  let textTexture = uniformTexture $ PixelRGBA8 r g b 255
  withTexture textTexture $
    printTextAt font (PointSize fontSize) position text

addTextToImage :: Image PixelRGBA8 -> Font -> Float -> V2 Float -> String -> PixelRGB8 -> Image PixelRGBA8
addTextToImage img font fontSize position text textColor =
  renderDrawing (imageWidth img) (imageHeight img) backgroundColor $ do
    drawImage img 0 (V2 0 0)
    drawText font fontSize position text textColor
  where
    backgroundColor = PixelRGBA8 0 0 0 255

overlayImages :: Image PixelRGBA8 -> Image PixelRGBA8 -> Point -> Image PixelRGBA8
overlayImages baseImage overlayImage position =
  renderDrawing (imageWidth baseImage) (imageHeight baseImage) backgroundColor $ do
    drawImage baseImage 0 (V2 0 0)
    drawImage overlayImage 0 position
  where
    backgroundColor = PixelRGBA8 0 0 0 0

generateImg :: Int -> Int -> PixelRGBA8 -> Image PixelRGBA8
generateImg width height backgroundColor =
  renderDrawing width height backgroundColor $
    withTexture (uniformTexture backgroundColor) $ do
      fill $ circle (V2 0 0) 30
