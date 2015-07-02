{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, OverloadedStrings, LambdaCase, TemplateHaskell, RankNTypes #-}

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Reader

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (asyncBound, cancel)
import Control.Concurrent.MVar (MVar)

import Linear
import Linear.Affine
import Foreign.C.Types
import Data.StateVar (($=))
import qualified SDL
import SDL (Rectangle(..))

import Data.Word (Word8)
import Data.Array (Array, (!), listArray)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Monoid
import Data.Maybe

import Control.Monad.Random
import Utils

type Color = V4 Word8
-- Gnome terminal "Solarized" palette
palette :: Array Int Color
palette = listArray (0, 15)
    [ genColor 0x07 0x36 0x42 --  0 dark blue
    , genColor 0xDC 0x32 0x2F --  1 red
    , genColor 0x85 0x99 0x00 --  2 green
    , genColor 0xB5 0x89 0x00 --  3 yellow
    , genColor 0x26 0x8B 0xD2 --  4 light blue
    , genColor 0xD3 0x36 0x82 --  5 pink
    , genColor 0x2A 0xA1 0x98 --  6 turqoise
    , genColor 0xEE 0xE8 0xD5 --  7 light beige
    , genColor 0x00 0x2B 0x36 --  8 darkest blue
    , genColor 0xCB 0x4B 0x16 --  9 orange
    , genColor 0x58 0x6E 0x75 -- 10 dark grey
    , genColor 0x65 0x7B 0x83 -- 11 grey
    , genColor 0x83 0x94 0x96 -- 12 light grey
    , genColor 0x6C 0x71 0xC4 -- 13 violet
    , genColor 0x93 0xA1 0xA1 -- 14 lightest grey
    , genColor 0xFD 0xF6 0xE3 -- 15 white
    ]
  where
    genColor r g b = V4 r g b maxBound

data Direction
    = DirUp
    | DirLeft
    | DirDown
    | DirRight
    deriving (Show, Eq)

data Snake = Snake
    { _snakeDirection :: Direction
    , _snakeSegments  :: [Rectangle CInt]
    , _snakeDead      :: Bool
    } deriving (Show)

data FoodType
    = FoodGreen
    | FoodOrange
    | FoodRed
    deriving (Show, Eq, Ord)

data Food = Food
    { _foodType     :: FoodType
    , _foodPos      :: Point V2 CInt
    , _foodLifetime :: Int
    } deriving (Show, Eq, Ord)

data Wall = Wall
    { _wall :: Rectangle CInt
    } deriving (Show)

data Board = Board
    { _boardScore        :: Int
    , _boardPendingScore :: Int
    , _boardPause        :: Bool
    , _boardSnake        :: Snake
    , _boardFood         :: Set Food
    , _boardWalls        :: [Wall]
    } deriving (Show)

data GameState = GameState
    { _gameStateBoard  :: MVar Board
    , _gameStateEvents :: MVar [SDL.EventPayload]
    }

makeLenses ''Snake
makeLenses ''Food
makeLenses ''Wall
makeLenses ''Board
makeLenses ''GameState

foodRect :: Food
         -> Rectangle CInt
foodRect f = Rectangle (f ^. foodPos) (V2 1 1)

tileSize :: Num a
         => a
tileSize = 16

windowSize, boardSize :: V2 CInt
windowSize = boardSize * tileSize
boardSize = V2 50 50

foodScore :: FoodType
          -> Int
foodScore FoodGreen  = 1
foodScore FoodOrange = 3
foodScore FoodRed    = 5

secondsPerStep :: Double
secondsPerStep = 1.0 / fromIntegral stepsPerSecond

stepsPerSecond :: Int
stepsPerSecond = 8

defaultBoard :: Board
defaultBoard = Board { _boardScore        = 0
                     , _boardPendingScore = 0
                     , _boardPause = False
                     , _boardSnake = Snake { _snakeDirection = DirUp
                                           , _snakeSegments = [Rectangle (P $ V2 (boardSize ^. _x `div` 2) (boardSize ^. _y `div` 2)) (V2 0 1)]
                                           , _snakeDead = False
                                           }
                     , _boardFood  = S.empty
                     , _boardWalls = [ Wall $ Rectangle (P $ V2 0 0) (V2 1 (boardSize ^. _y))
                                     , Wall $ Rectangle (P $ V2 0 0) (V2 (boardSize ^. _x) 1)
                                     , Wall $ Rectangle (P $ V2 (boardSize ^. _x - 1) 0) (V2 1 (boardSize ^. _y))
                                     , Wall $ Rectangle (P $ V2 0 (boardSize ^. _y - 1)) (V2 (boardSize ^. _x) 1)
                                     , Wall $ Rectangle (P $ V2 (boardSize ^. _x `div` 4) (boardSize ^. _y `div` 4)) (V2 1 (boardSize ^. _y - boardSize ^. _y `div` 2))
                                     , Wall $ Rectangle (P $ V2 (boardSize ^. _x - boardSize ^. _x `div` 4) (boardSize ^. _y `div` 4)) (V2 1 (boardSize ^. _y - boardSize ^. _y `div` 2))
                                     ]
                     }

class Collidable a b where
    collides :: a
             -> b
             -> Bool

instance (Num a, Ord a) => Collidable (Rectangle a) (Rectangle a) where
    collides = collidesRect

collidesRect :: (Num a, Ord a)
             => Rectangle a
             -> Rectangle a
             -> Bool
collidesRect (Rectangle a ab) (Rectangle a' ab')
     | a ^. _x >= b' ^. _x = False -- second is on the left side of first
     | b ^. _x <= a' ^. _x = False -- second is on the right side of first
     | a ^. _y >= b' ^. _y = False -- second is on the top side of first
     | b ^. _y <= a' ^. _y = False -- second is on the bottom side of first
     | otherwise           = True
  where
    b  = a  .+^ ab
    b' = a' .+^ ab'

instance Collidable Food (Rectangle CInt) where
    collides f = collides (foodRect f)

instance Collidable (Rectangle CInt) Food where
    collides = flip collides

instance Collidable Wall (Rectangle CInt) where
    collides w = collides (w ^. wall)

instance Collidable (Rectangle CInt) Wall where
    collides = flip collides

instance Collidable Food Wall where
    collides f = collides (foodRect f)

instance Collidable Wall Food where
    collides = flip collides

collidesAny :: (Collidable c d, Foldable t)
            => c
            -> t d
            -> Bool
collidesAny c = getAny . foldMap (Any . collides c)

directionVector :: Num a
                => Direction
                -> V2 a
directionVector DirLeft  = V2 (-1)   0
directionVector DirRight = V2   1    0
directionVector DirUp    = V2   0  (-1)
directionVector DirDown  = V2   0    1

moveSnake :: Num a
          => Direction
          -> Direction
          -> Rectangle a
          -> Rectangle a
moveSnake oldDirection newDirection oldSegment = uncurry Rectangle $
    case (oldDirection, newDirection) of
        -- same direction
        (o       , n       ) | directionVector o ==   directionVector n  -> (a .+^ directionVector n, ab')
        -- reverse direction
        (o       , n       ) | directionVector o == -(directionVector n) -> (a .-^ directionVector n, ab')
        -- other cases
        (DirRight, DirDown )   -> (b .-^ V2 1 0, ab')
        (DirRight, DirUp   )   -> (b .-^ V2 1 2, ab')
        (DirLeft , DirDown )   -> (a .+^ V2 0 1, ab')
        (DirLeft , DirUp   )   -> (a .-^ V2 0 1, ab')
        (DirUp   , DirRight)   -> (a .+^ V2 1 0, ab')
        (DirUp   , DirLeft )   -> (a .-^ V2 1 0, ab')
        (DirDown , DirRight)   -> (b .-^ V2 0 1, ab')
        (DirDown , DirLeft )   -> (b .-^ V2 2 1, ab')
        _                      -> error "unmatched direction change" -- can't happen but the compiler does not know
  where
    Rectangle a ab = oldSegment
    b              = a .+^ ab
    ab'            = V2 1 1

gameStep :: MonadRandom m
         => [SDL.EventPayload]
         -> Board
         -> m Board
gameStep events board | pausePressed                       = return (board & boardPause %~ not)
                      | resetPressed                       = return defaultBoard
                      | board ^. boardPause                = return board
                      | board ^. boardSnake . snakeDead    = return board
                      | otherwise                          = generateNextFrame board newDirection
  where
    keyPressed k = not . null $
                        filter (\case SDL.KeyboardEvent e
                                        | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                                            SDL.keysymKeycode (SDL.keyboardEventKeysym e) == k
                                      _ -> False)
                            events

    pausePressed = keyPressed SDL.KeycodeSpace
    resetPressed = keyPressed SDL.KeycodeR

    newDirection = fromMaybe (board ^. boardSnake . snakeDirection) . getLast $
                       foldMap (\case SDL.KeyboardEvent e
                                        | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                                            case SDL.keysymKeycode (SDL.keyboardEventKeysym e) of
                                                SDL.KeycodeUp    -> Last (Just DirUp)
                                                SDL.KeycodeDown  -> Last (Just DirDown)
                                                SDL.KeycodeRight -> Last (Just DirRight)
                                                SDL.KeycodeLeft  -> Last (Just DirLeft)
                                                _                -> mempty
                                      _                                                -> mempty)
                            events

generateNextFrame :: MonadRandom m
                  => Board
                  -> Direction
                  -> m Board
generateNextFrame board newDirection = do
    let snake        = board ^. boardSnake
        oldDirection = snake ^. snakeDirection

        oldSnakeSegments = snake ^. snakeSegments
        newSnakeSegment  = moveSnake oldDirection newDirection (head oldSnakeSegments)
        newSnakeSegments = newSnakeSegment : oldSnakeSegments

        -- Check if any segment collides with the new one
        selfCollide      = collidesAny newSnakeSegment oldSnakeSegments

        -- Colliding with any walls?
        wallCollide      = collidesAny newSnakeSegment (board ^. boardWalls)

        -- We're dead if colliding with ourselves or any walls
        nowDead          = selfCollide || wallCollide

        -- Check if we gathered some food and calculate the score
        foodCollide      = filter (collides newSnakeSegment) $ S.toList (board ^. boardFood)

        score            = sum . fmap (foodScore . view foodType) $ foodCollide
        pendingScore     = (board ^. boardPendingScore) + score

        -- Build our new snake
        newSnake = snake & snakeSegments  .~ (if      nowDead          then oldSnakeSegments
                                              else if pendingScore > 0 then newSnakeSegments
                                              else                          init newSnakeSegments)
                         & snakeDirection .~ newDirection
                         & snakeDead      %~ (|| nowDead)

        -- Remove all consumed food, expire food and remove expired food
        prunedFood = S.filter ((> 0) . view foodLifetime)
                        . S.map (over foodLifetime pred) $
                            S.difference (board ^. boardFood) (S.fromList foodCollide)


    -- Generate new random food if there is none left
    newFood <- if S.null prunedFood then do
                    foodTypeRnd <- getUniformR (0.0, 1.0 :: Double)
                    foodNewLifetime <- getUniformR (10 * stepsPerSecond, 100 * stepsPerSecond)
                    let rndFood = do
                            foodPosX <- (fromIntegral :: Int -> CInt) <$> getUniformR (0, fromIntegral $ boardSize ^. _x - 1)
                            foodPosY <- (fromIntegral :: Int -> CInt) <$> getUniformR (0, fromIntegral $ boardSize ^. _y - 1)

                            let newFood  = Food { _foodPos  = P $ V2 foodPosX foodPosY
                                                , _foodType = if      foodTypeRnd <= 0.05 then FoodRed
                                                              else if foodTypeRnd <= 0.1  then FoodOrange
                                                              else                             FoodGreen
                                                , _foodLifetime = foodNewLifetime}

                                sCollide = collidesAny newFood (newSnake ^. snakeSegments)
                                wCollide = collidesAny newFood (board ^. boardWalls)

                            -- Collision with snake or walls? Try again!
                            if sCollide || wCollide then
                                rndFood
                            else
                                return newFood

                    newFood <- rndFood
                    return (S.insert newFood prunedFood)
               else
                    return prunedFood

    return $ board & boardSnake        .~ newSnake
                   & boardScore        %~ (+ score)
                   & boardFood         .~ newFood
                   & boardPendingScore .~ max 0 (pendingScore - 1)


gameLoop :: (MonadIO m, MonadRandom m, MonadMask m, MonadReader GameState m)
         => Integer
         -> m ()
gameLoop prevTime = do
    gameState <- ask
    -- Get events and replace them by the empty list
    events <- modifyMVar (gameState ^. gameStateEvents) (return . (,) [])
    -- Do one game step
    modifyMVar_ (gameState ^. gameStateBoard) (gameStep events)

    -- Calculate when we should do the next step
    currentTime <- getCurrentMonotonicTime
    let nextTime   = prevTime + floor (secondsPerStep * 1000.0) * 1000 * 1000
        waitTime   = nextTime - currentTime
        waitTimeUs = fromIntegral $ waitTime `div` 1000

    -- Wait until the next step if we're not late
    when (waitTimeUs > 0) $
        liftIO $ threadDelay waitTimeUs

    gameLoop nextTime

runGameLoop :: GameState
            -> IO ()
runGameLoop gameState = do
    startTime <- getCurrentMonotonicTime
    gen <- createSystemRandom
    runReaderT (runRandT (gameLoop startTime) gen) gameState

class Renderable a where
    render :: MonadIO m
           => SDL.Renderer
           -> a
           -> m ()

instance Renderable Snake where
    render = renderSnake

instance Renderable Wall where
    render = renderWall

instance Renderable Food where
    render = renderFood

instance Renderable Board where
    render = renderBoard

instance (Foldable t, Renderable a) => Renderable (t a) where
    render r = mapM_ (render r)

renderSnake :: MonadIO m
            => SDL.Renderer
            -> Snake
            -> m ()
renderSnake renderer snake = do
    SDL.renderDrawColor renderer $= palette ! (if snake ^. snakeDead then 5 else 4)
    forM_ (snake ^. snakeSegments) $
        SDL.renderFillRect renderer . Just . fmap (* tileSize)

renderWall :: MonadIO m
           => SDL.Renderer
           -> Wall
           -> m ()
renderWall renderer w = do
    SDL.renderDrawColor renderer $= palette ! 6
    SDL.renderFillRect renderer . Just . fmap (* tileSize) $ w ^. wall

renderFood :: MonadIO m
           => SDL.Renderer
           -> Food
           -> m ()
renderFood renderer food = do
    SDL.renderDrawColor renderer $= case food ^. foodType of
                                         FoodGreen   -> palette ! 2
                                         FoodOrange  -> palette ! 9
                                         FoodRed     -> palette ! 1
    SDL.renderFillRect renderer . Just . fmap (* tileSize) $ foodRect food

renderBoard :: MonadIO m
            => SDL.Renderer
            -> Board
            -> m ()
renderBoard renderer board = do
    render renderer (board ^. boardSnake)
    render renderer (board ^. boardFood)
    render renderer (board ^. boardWalls)

runRenderLoop :: (MonadIO m, MonadMask m, MonadReader (SDL.Renderer, GameState) m)
              => m ()
runRenderLoop = do
    events <- SDL.pollEvents

    let eventPayloads = fmap SDL.eventPayload events
        quit = any (\case SDL.QuitEvent -> True
                          _             -> False) eventPayloads

    (renderer, gameState) <- ask
    modifyMVar_ (gameState ^. gameStateEvents) (return . (<> eventPayloads))

    SDL.renderDrawColor renderer $= palette ! 8
    SDL.renderClear renderer

    board <- readMVar (gameState ^. gameStateBoard)
    render renderer board

    SDL.renderPresent renderer

    unless quit runRenderLoop

main :: IO ()
main = do
    SDL.initialize [SDL.InitVideo]

    window <- SDL.createWindow
                "Snake"
                SDL.defaultWindow
    SDL.windowSize window $= windowSize
    SDL.showWindow window

    renderer <- SDL.createRenderer
                    window
                    (-1)
                    SDL.defaultRenderer
                        { SDL.rendererType = SDL.AcceleratedVSyncRenderer
                        }

    boardMVar <- newMVar defaultBoard
    eventMVar <- newMVar []

    let gameState = GameState
                        { _gameStateBoard  = boardMVar
                        , _gameStateEvents = eventMVar
                        }

    gameThread <- asyncBound $ runGameLoop gameState
    runReaderT runRenderLoop (renderer, gameState)

    cancel gameThread

    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

