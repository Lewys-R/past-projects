module KarmaBrief where

import System.Random
import Control.Monad.State
import Data.List
import Data.Ord
import Data.Maybe (listToMaybe)

-- Cards
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK | RA
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Eq, Show, Read)

type Deck = [Card]
type Pile = [Card]

-- Strategies
data Strategy = Basic | BasicSets | Smart
  deriving (Eq, Show)

-- Players
type PlayerId   = Int
type PlayerName = String

data Player = Player
  { pId       :: PlayerId
  , pName     :: PlayerName
  , strategy  :: Strategy
  , hand      :: [Card]
  , faceUp    :: [Card]
  , faceDown  :: [Card]
  } deriving (Show)

-- Game state 
data GameState = GameState
  { players       :: [Player]
  , currentIx     :: Int         -- Index of current active player
  , drawPile      :: Deck
  , discardPile   :: Pile
  , burnedPiles   :: [Pile]
  , turnNumber    :: Int         -- Accumulating counter of turns
  , rng           :: StdGen      -- Random number generator
  , finishedOrder :: [PlayerId]
  , extensions :: [Extension]
  , directionOfPlay :: Int       -- 1 for clockwise, -1 for counterclockwise
  } deriving (Show)

-- Different extension rules we can toggle
data Extension = ExtReverse8 | ExtThree3s | ExtNineClubs
  deriving (Eq, Show)

-- Number of turns after which the game is forcibly terminated
maxTurns :: Int
maxTurns = 750

-- String to output when a game runs for longer than acceptible
terminateMessage :: String
terminateMessage =
  "\nThe number of turns performed by players exceeded 750 turns. The game has been terminated.\n"

--------------------------------------------------------------------------------
-- Step 1 
--------------------------------------------------------------------------------

legalPlay :: Maybe Card -> Card -> Bool
legalPlay Nothing _ = True
legalPlay _ (Card R2 _) = True -- 'Reset' pile upon rank two
legalPlay (Just topCard) inputCard
  | rank topCard == R7 = rank inputCard <= rank topCard -- Invert legality upon rank seven
  | otherwise = rank inputCard >= rank topCard

validPlays :: Maybe Card -> Deck -> Deck
validPlays _ [] = []
validPlays topCard (x:xs)
  | legalPlay topCard x = x : validPlays topCard xs
  | otherwise = validPlays topCard xs

dealCards :: Int -> State GameState Deck
dealCards nCardsToDeal = do
  gameState <- get

  -- Draw and remove cards from the draw pile's first three indexes
  let currentDrawPile = drawPile gameState
      dealtCards = take nCardsToDeal currentDrawPile
      remainingCards = drop nCardsToDeal currentDrawPile

  -- Update the game state with new draw pile
  put gameState { drawPile = remainingCards }

  return dealtCards

giveWastePileTo :: Player -> State GameState ()
giveWastePileTo player = do
  gameState <- get

  case find (\p -> pId p == pId player) (players gameState) of
    Nothing -> error "ERROR: Player does not exist in the game state."

    Just _ -> do

      -- Find and update the player's hand
      let updatedPlayerList = map
            (\p -> if pId p == pId player
              then p { hand = hand p ++ discardPile gameState }
              else p)
            (players gameState)

      -- Update the game state
      put gameState { players = updatedPlayerList, discardPile = [] }

replenishCards :: Player -> State GameState ()
replenishCards player = do
  gameState <- get

  case find (\p -> pId p == pId player) (players gameState) of
    Nothing -> error "ERROR: Player does not exist in the game state."

    Just currentPlayer -> do

      -- Find and update the player's hand
      let playerHand = hand currentPlayer
          cardsToDeal = max 0 (3 - length playerHand)

      dealtCards <- dealCards cardsToDeal

      gameState <- get

      let updatedPlayerList = map (\p ->
              if pId p == pId player
                then p { hand = playerHand ++ dealtCards }
                else p
            ) (players gameState)

      -- Update the game state
      put gameState { players = updatedPlayerList }

shuffleDeck :: StdGen -> Deck -> Deck
shuffleDeck _ [] = []
shuffleDeck generator inputDeck =
  let
    (index, generator') = randomR (0, length inputDeck - 1) generator
    shuffledCards = inputDeck !! index
    remainingCards = take index inputDeck ++ drop (index + 1) inputDeck
  in
    shuffledCards : shuffleDeck generator' remainingCards

--------------------------------------------------------------------------------
-- Step 2 
--------------------------------------------------------------------------------

basicStrategy :: State GameState Deck
basicStrategy = do
  gameState <- get

  let currentPlayer = players gameState !! currentIx gameState

      currentHand = sortOn rank (hand currentPlayer)
      currentFaceUp = sortOn rank (faceUp currentPlayer)
      currentFaceDown = faceDown currentPlayer

      effectiveTopCard = find (\card -> rank card /= R8) (discardPile gameState)

      -- Play the lowest ranking valid card from the current player's hand cards
      playFromHand = pure $ case validPlays effectiveTopCard currentHand of
        [] -> []
        (card:_) -> [card]

      -- Play the lowest ranking valid card from the current player's face-up cards
      playFromFaceUp = pure $ case validPlays effectiveTopCard currentFaceUp of
        [] -> []
        (card:_) -> [card]

      -- Play a random hidden card from the current player's face-down cards
      playFromFaceDown = case currentFaceDown of
        [] -> pure []
        faceDownCards ->
          let (randomIx, generator') = randomR (0, length faceDownCards - 1) (rng gameState)
              card = faceDownCards !! randomIx
          in do
            put gameState { rng = generator' }
            pure ([card | card `elem` validPlays effectiveTopCard [card]])

      cardToPlay
        | not (null currentHand) = playFromHand -- Stage 1
        | not (null currentFaceUp) && null (drawPile gameState) = playFromFaceUp -- Stage 2
        | not (null currentFaceDown) = playFromFaceDown -- Stage 3
        | otherwise = error "ERROR: An error occurred when attempting to choose a card to play."

  cardToPlay

applyStrategy :: State GameState ()
applyStrategy = do
  gameState <- get

  let currentPlayer = players gameState !! currentIx gameState
      currentStrategy = strategy currentPlayer

  -- Apply the player's respective strategy
  cardsToPlay <- case currentStrategy of
    Basic -> basicStrategy
    BasicSets -> basicStrategySets
    Smart -> smartStrategy

  -- Reverse the direction of play upon a played card of rank eight, if extension is enabled
  if ExtReverse8 `elem` extensions gameState && not (null cardsToPlay) &&
      rank (head cardsToPlay) == R8
    then put gameState { directionOfPlay = negate (directionOfPlay gameState) }
    else put gameState

  gameState <- get

  let remainingPlayers = filter (\p ->
          not (null (hand p)) ||
          not (null (faceUp p)) ||
          not (null (faceDown p))
        ) (players gameState)

      direction = directionOfPlay gameState
      stepIx ix = (ix + direction + length (players gameState)) `mod` length (players gameState)

      findIx ix
        | pId (players gameState !! ix) `notElem` finishedOrder gameState = ix
        | ix == currentIx gameState = error "ERROR: All players exist in finishedOrder."
        | otherwise = findIx (stepIx ix)

      nextIx = findIx (stepIx (currentIx gameState))

  gameState <- get

  -- Handle the case wherein no valid play can be made
  if null cardsToPlay then do

    giveWastePileTo currentPlayer
    gameState <- get

    replenishCards currentPlayer
    gameState <- get

    put gameState { currentIx = nextIx }

    -- Handle the case wherein a valid play can be made
    else do
      let playedCards = cardsToPlay

          -- Add the played card(s) to the discard pile
          discardPileWithPlayedCards = playedCards ++ discardPile gameState

          -- Function to remove given cards from a list
          removeCardsFrom :: [Card] -> [Card] -> [Card]
          removeCardsFrom cardsToRemove list
            | allOfEqualRank cardsToRemove = foldr delete list cardsToRemove
            | otherwise = error "ERROR: Cards to remove were of various ranks."
            where
              allOfEqualRank [] = True
              allOfEqualRank (x:xs) = all (\card -> rank card == rank x) xs

          -- Remove the played card(s) from the player's inventory
          updatedPlayer
            | all (`elem` hand currentPlayer) playedCards =
              currentPlayer { hand = removeCardsFrom playedCards (hand currentPlayer) }
            | all (`elem` faceUp currentPlayer) playedCards =
              currentPlayer { faceUp = removeCardsFrom playedCards (faceUp currentPlayer) }
            | all (`elem` faceDown currentPlayer) playedCards =
              currentPlayer { faceDown = removeCardsFrom playedCards (faceDown currentPlayer) }
            | otherwise = error "ERROR: Played cards do not exist in the current player's inventory."

          -- Create the list of all players, including the newly updated player
          updatedPlayerList = map (\p ->
              if pId p == pId updatedPlayer then updatedPlayer else p
            ) (players gameState)

          -- Check if a burn is caused by any played card or after-play effect
          burnOccurs :: Bool
          burnOccurs =
            let pile = discardPileWithPlayedCards

                effectiveTopCard = find (\card -> rank card /= R8) pile

                burnByRankTen = case effectiveTopCard of
                  Just card -> rank card == R10
                  Nothing -> False

                effectiveTopFourCards = take 4 (filter (\card -> rank card /= R8) pile)

                burnByConsecutiveFour =
                  case effectiveTopCard of
                    Just topCard ->
                      length effectiveTopFourCards >= 4 &&
                      all (\card -> rank card == rank topCard) (tail effectiveTopFourCards)
                    Nothing -> False

                burnByConsecutiveEight = length (takeWhile (\card -> rank card == R8) pile) >= 4

            in burnByRankTen || burnByConsecutiveFour || burnByConsecutiveEight

          -- Apply "burn" after-play effect, if warranted
          (updatedDiscardPile, updatedBurnedPiles) =
            if burnOccurs
              then ([], burnedPiles gameState ++ [discardPileWithPlayedCards])
              else (discardPileWithPlayedCards, burnedPiles gameState)

          -- Condition to force the next player to pick up the discard pile upon three threes
          forcePickup =
            ExtThree3s `elem` extensions gameState &&
            not (null (discardPile gameState)) &&
            not burnOccurs &&
            length playedCards == 3 &&
            all (\card -> rank card == R3) playedCards

          -- Force the next player to pick up the discard pile, if condition is satisfied
          (nextUpdatedPlayerList, nextUpdatedDiscardPile) =
            if forcePickup
              then
                let nextPlayer = updatedPlayerList !! nextIx
                    updatedNextPlayer =
                      nextPlayer { hand = hand nextPlayer ++ updatedDiscardPile }
                    players' = map (\p ->
                      if pId p == pId updatedPlayer
                        then updatedPlayer
                        else if pId p == pId nextPlayer
                          then updatedNextPlayer
                          else p) (players gameState)
                in (players', [])
              else (updatedPlayerList, updatedDiscardPile)

          -- Condition to allow the current player to steal a card from the next player
          forceSteal =
            ExtNineClubs `elem` extensions gameState &&
            any (\card -> rank card == R9 && suit card == Clubs) playedCards

          -- Apply 'steal' after-play effect, if warranted
          (finalUpdatedPlayerList, finalUpdatedDiscardPile, finalRNG) =
            if forceSteal && not (null (hand (nextUpdatedPlayerList !! nextIx)))
              then
                let nextPlayer = nextUpdatedPlayerList !! nextIx
                    (ixToSteal, generator') = randomR (0, length (hand nextPlayer) - 1) (rng gameState)
                    cardToSteal = [hand nextPlayer !! ixToSteal]
                    updatedNextPlayer = nextPlayer {
                        hand = removeCardsFrom cardToSteal (hand nextPlayer)
                      }
                    updatedCurrentPlayer = updatedPlayer { hand = hand updatedPlayer ++ cardToSteal }
                    updatedPlayers = map (\p ->
                      if pId p == pId updatedCurrentPlayer
                        then updatedCurrentPlayer
                        else if pId p == pId updatedNextPlayer
                          then updatedNextPlayer
                          else p) nextUpdatedPlayerList
                in (updatedPlayers, nextUpdatedDiscardPile, generator')
              else (nextUpdatedPlayerList, nextUpdatedDiscardPile, rng gameState)

      put gameState {
        players = finalUpdatedPlayerList,
        rng = finalRNG
      }

      gameState <- get

      let finalCurrentPlayer = head (filter (\p -> pId p == pId currentPlayer) (players gameState))

      replenishCards finalCurrentPlayer

      gameState <- get

      -- Update finished order accordingly
      let currentPlayer = players gameState !! currentIx gameState

          exitCondition = null (hand currentPlayer) && null (faceUp currentPlayer)
            && null (faceDown currentPlayer)

          updatedFinishedOrder = if exitCondition && pId currentPlayer `notElem` finishedOrder gameState
            then finishedOrder gameState ++ [pId currentPlayer]
            else finishedOrder gameState

      put gameState {
        currentIx = nextIx,
        discardPile = finalUpdatedDiscardPile,
        burnedPiles = updatedBurnedPiles,
        turnNumber = turnNumber gameState + 1,
        finishedOrder = updatedFinishedOrder
      }

gameLoop :: State GameState String
gameLoop = do
  gameState <- get

  -- Terminate the game if it has taken too long to finish
  if turnNumber gameState > maxTurns
    then
      return terminateMessage
    else do

      let remainingPlayers = filter (\p ->
              not (null (hand p)) ||
              not (null (faceUp p)) ||
              not (null (faceDown p))
            ) (players gameState)

      if length remainingPlayers <= 1
        then do

          -- Output summary message detailing the winners and losers when the game is finished
          let playerFromId playerId = head (filter (\p -> pId p == playerId) (players gameState))
              winners = map (pName . playerFromId) (finishedOrder gameState)
              loser = map pName (filter (\p -> pId p `notElem` finishedOrder gameState) (players gameState))
              summaryMessage =
                "The winner(s) are: " ++ unwords winners ++
                "\nThe loser is: " ++ head loser

          return summaryMessage

        else do

          -- Continue playing with the player's strategy
          applyStrategy

          gameState <- get

          -- Recurse until the game is finished
          gameLoop

runOneGame :: IO ()
runOneGame = do

  -- Initialise a GameState and its properties
  let generator = mkStdGen 46

      -- Create the draw pile from which to play
      initialDrawPile = shuffleDeck generator [ Card rank suit |
          suit <- [Hearts, Spades, Diamonds, Clubs],
          rank <- [R2 .. RA]
        ]

      -- Initialise the players
      unpopulatedPlayers = [
          Player 1 "Ffion" Basic [] [] [],
          Player 2 "Rhys" Basic [] [] [],
          Player 3 "Gwen" Basic [] [] []
        ]

      -- Initialise a game state wherein players have yet to possess cards
      unpopulatedGameState = GameState {
          players = unpopulatedPlayers,
          currentIx = 0,
          drawPile = initialDrawPile,
          discardPile = [],
          burnedPiles = [],
          turnNumber = 1,
          rng = generator,
          finishedOrder = [],
          extensions = [],
          directionOfPlay = 1
        }

      -- Deal three cards to each player's hand, face-up, and face-down list
      populatedGameState = execState (do
          gameState <- get

          initialPlayers <- mapM (\p -> do
              handCards <- dealCards 3
              faceUpCards <- dealCards 3
              faceDownCards <- dealCards 3
              return p { hand = handCards, faceUp = faceUpCards, faceDown = faceDownCards }
            ) (players gameState)

          gameState <- get

          put gameState { players = initialPlayers }

        ) unpopulatedGameState

      -- Assign the game state's current index to that of the chosen starting player
      initialGameState = execState chooseStartingPlayer populatedGameState

      -- Evalute the game's results
      summaryMessage = evalState gameLoop initialGameState

  putStrLn summaryMessage

chooseStartingPlayer :: State GameState ()
chooseStartingPlayer = do
  gameState <- get

  let playersWithCurrentRank currentRank =
        filter (any (\card -> rank card == currentRank) . hand) (players gameState)

      eligiblePlayers = listToMaybe [
          playersWithCurrentRank currentRank | currentRank <- [R3 .. RA],
          not (null (playersWithCurrentRank currentRank))
        ]

  -- Select the starting player based on the setup rules of Karma
  startingPlayer <- case eligiblePlayers of
    Nothing -> error "An error occurred when attempting to choose the starting player."
    Just [singlePlayer] -> return singlePlayer
    Just manyPlayers -> do
      let (randomIx, generator') = randomR (0, length manyPlayers - 1) (rng gameState)
      put gameState { rng = generator' }
      return (manyPlayers !! randomIx)

  gameState <- get

  let Just startingIx = findIndex (\p -> pId p == pId startingPlayer) (players gameState)

  put gameState {
    currentIx = startingIx
  }

--------------------------------------------------------------------------------
-- Step 3 
--------------------------------------------------------------------------------

basicStrategySets:: State GameState Deck
basicStrategySets = do
  gameState <- get

  let currentPlayer = players gameState !! currentIx gameState

      currentHand = sortOn rank (hand currentPlayer)
      currentFaceUp = sortOn rank (faceUp currentPlayer)
      currentFaceDown = faceDown currentPlayer

      effectiveTopCard = find (\card -> rank card /= R8) (discardPile gameState)

      -- Play all legal cards of the lowest rank from the player's hand cards
      playFromHand = pure $ case validPlays effectiveTopCard currentHand of
        [] -> []
        (c:cs) -> c : takeWhile (\currentCard -> rank currentCard == rank c) cs

      -- Play all legal cards of the lowest rank from the player's face-up cards
      playFromFaceUp = pure $ case validPlays effectiveTopCard currentFaceUp of
        [] -> []
        (c:cs) -> c : takeWhile (\currentCard -> rank currentCard == rank c) cs

      -- Play a random hidden card from the current player's face-down cards
      playFromFaceDown = case currentFaceDown of
        [] -> pure []
        faceDownCards ->
          let (randomIx, generator') = randomR (0, length faceDownCards - 1) (rng gameState)
              card = faceDownCards !! randomIx
          in do
            put gameState { rng = generator' }
            pure ([card | card `elem` validPlays effectiveTopCard [card]])

      cardsToPlay
        | not (null currentHand) = playFromHand -- Stage 1
        | not (null currentFaceUp) && null (drawPile gameState) = playFromFaceUp -- Stage 2
        | not (null currentFaceDown) = playFromFaceDown -- Stage 3
        | otherwise = error "ERROR: An error occurred when attempting to choose a card to play."

  cardsToPlay

gameLoopWithHistory :: State GameState String
gameLoopWithHistory = do
  gameState <- get

  let remainingPlayers = filter (\p ->
          not (null (hand p)) ||
          not (null (faceUp p)) ||
          not (null (faceDown p))
        ) (players gameState)

  if length remainingPlayers <= 1
    then do

      -- Output summary message detailing the winners and losers when the game is finished
      let playerFromId playerId = head (filter (\p -> pId p == playerId) (players gameState))
          winners = map (pName . playerFromId) (finishedOrder gameState)
          loser = map pName (filter (\p -> pId p `notElem` finishedOrder gameState) (players gameState))
          summaryMessage = "\n=== RESULTS ===\n\n" ++
            "The winner(s) are: " ++ unwords winners ++
            "\nThe loser is: " ++ head loser ++ "\n"

      return summaryMessage

    else do

      let playerBeforePlay = players gameState !! currentIx gameState
          effectiveTopCard = find (\card -> rank card /= R8) (discardPile gameState)

          -- Create message detailing the turn number
          turnNumberMessage = "\n========== TURN " ++ show (turnNumber gameState) ++ " ==========\n"

          -- Create message detailing the player's cards before they play
          beforeTurnPlayerMessage = "\n===== BEFORE =====\n\n" ++
            pName playerBeforePlay ++ "'s Cards:" ++
            "\n\nHand: " ++ show (sortOn rank (hand playerBeforePlay)) ++
            "\nFace Up: " ++ show (sortOn rank (faceUp playerBeforePlay)) ++
            "\nFace Down: " ++ show (length (faceDown playerBeforePlay))

          -- Create message detailing the discard pile before a play has been made
          beforeTurnDiscardPileMessage = case discardPile gameState of
            [] -> "\nThe discard pile is empty, or has been burned."
            pile ->
              "\nThe entire discard pile is: " ++ show pile ++
              "\nThe top card of the discard pile is: " ++ show (head pile) ++
              case effectiveTopCard of
                Just effectiveTopCard | rank effectiveTopCard /= rank (head pile) ->
                  "\nThe effective top card of the discard pile is: " ++ show effectiveTopCard
                _ -> ""

      -- Continue playing with chosen strategy
      applyStrategy

      gameState <- get

      let playerAfterPlay = head $ filter (\p -> pId p == pId playerBeforePlay) (players gameState)
          effectiveTopCard = find (\card -> rank card /= R8) (discardPile gameState)

          -- Create message detailing the player's cards after they've played
          afterTurnPlayerMessage = "\n===== AFTER =====\n\n" ++
            pName playerAfterPlay ++ "'s Cards:" ++
            "\n\nHand: " ++ show (sortOn rank (hand playerAfterPlay)) ++
            "\nFace Up: " ++ show (sortOn rank (faceUp playerAfterPlay)) ++
            "\nFace Down: " ++ show (length (faceDown playerAfterPlay))

          -- Create message detailing the discard pile after a play has been made
          afterTurnDiscardPileMessage = case discardPile gameState of
            [] -> "\nThe discard pile is empty, or has been burned."
            pile ->
              "\nThe entire discard pile is: " ++ show pile ++
              "\nThe top card of the discard pile is: " ++ show (head pile) ++
              case effectiveTopCard of
                Just effectiveTopCard | rank effectiveTopCard /= rank (head pile) ->
                  "\nThe effective top card of the discard pile is: " ++ show effectiveTopCard
                _ -> ""

          -- Create message if the player is out of the game
          playerGetsOutMessage =
            if pId playerAfterPlay `elem` finishedOrder gameState
              then "\n" ++ pName playerAfterPlay ++
              " has exhausted all their cards and are out of the game."
              else ""

      let turnSummaryMessage = unlines [
              turnNumberMessage,
              beforeTurnPlayerMessage, beforeTurnDiscardPileMessage,
              afterTurnPlayerMessage, afterTurnDiscardPileMessage,
              playerGetsOutMessage
            ]

      return turnSummaryMessage

runOneGameWithHistory :: IO ()
runOneGameWithHistory = do

  -- Initialise a GameState and its properties
  let generator = mkStdGen 46

      -- Create the draw pile from which to play
      initialDrawPile = shuffleDeck generator [ Card rank suit |
          suit <- [Hearts, Spades, Diamonds, Clubs],
          rank <- [R2 .. RA]
        ]

      unpopulatedPlayers = [
          Player 1 "Ffion" Basic [] [] [],
          Player 2 "Rhys" Basic [] [] [],
          Player 3 "Gwen" BasicSets [] [] []
        ]

      -- Initialise a game state wherein players have yet to possess cards
      unpopulatedGameState = GameState {
          players = unpopulatedPlayers,
          currentIx = 0,
          drawPile = initialDrawPile,
          discardPile = [],
          burnedPiles = [],
          turnNumber = 1,
          rng = generator,
          finishedOrder = [],
          extensions = [],
          directionOfPlay = 1
        }

      -- Deal three cards to each player's hand, face-up, and face-down list
      populatedGameState = execState (do
          gameState <- get

          initialPlayers <- mapM (\p -> do
              handCards <- dealCards 3
              faceUpCards <- dealCards 3
              faceDownCards <- dealCards 3
              return p { hand = handCards, faceUp = faceUpCards, faceDown = faceDownCards }
            ) (players gameState)

          gameState <- get

          put gameState { players = initialPlayers }

        ) unpopulatedGameState

      -- Assign the game state's current index to that of the chosen starting player
      initialGameState = execState chooseStartingPlayer populatedGameState

      startingPlayer = players initialGameState !! currentIx initialGameState
      startingMessage = "\n=== STARTING PLAYER ===\n" ++ "\nPlayer Name: " ++ pName startingPlayer

      -- Repeat the game's loop until finished
      runGameLoop gameState = do

          -- Perform a single turn
          let (turnInfoMessage, updatedGameState) = runState gameLoopWithHistory gameState

          if turnNumber updatedGameState > maxTurns
            then putStrLn terminateMessage
            else do

              putStrLn turnInfoMessage

              let remainingPlayers = filter (\p ->
                      not (null (hand p)) ||
                      not (null (faceUp p)) ||
                      not (null (faceDown p))
                    ) (players updatedGameState)

              -- Check if the game has finished
              if length remainingPlayers > 1
                then runGameLoop updatedGameState
                else do
                  let (summaryMessage, _) = runState gameLoopWithHistory updatedGameState
                  putStrLn summaryMessage

  putStrLn startingMessage
  runGameLoop initialGameState

--------------------------------------------------------------------------------
-- Step 4 
--------------------------------------------------------------------------------

playOneGameStep4 :: [Extension] -> IO ()
playOneGameStep4 extensions = do

  -- Initialise a GameState and its properties
  let generator = mkStdGen 12

      -- Create the draw pile from which to play
      initialDrawPile = shuffleDeck generator [ Card rank suit |
          suit <- [Hearts, Spades, Diamonds, Clubs],
          rank <- [R2 .. RA]
        ]

      unpopulatedPlayers = [
          Player 1 "Dylan" Basic [] [] [],
          Player 2 "Carys" Basic [] [] [],
          Player 3 "Evan" BasicSets [] [] []
        ]

      -- Initialise a game state wherein players have yet to possess cards
      unpopulatedGameState = GameState {
          players = unpopulatedPlayers,
          currentIx = 0,
          drawPile = initialDrawPile,
          discardPile = [],
          burnedPiles = [],
          turnNumber = 1,
          rng = generator,
          finishedOrder = [],
          extensions = extensions,
          directionOfPlay = 1
        }

      -- Deal three cards to each player's hand, face-up, and face-down list
      populatedGameState = execState (do
          gameState <- get

          initialPlayers <- mapM (\p -> do
              handCards <- dealCards 3
              faceUpCards <- dealCards 3
              faceDownCards <- dealCards 3
              return p { hand = handCards, faceUp = faceUpCards, faceDown = faceDownCards }
            ) (players gameState)

          gameState <- get

          put gameState { players = initialPlayers }

        ) unpopulatedGameState

      -- Assign the game state's current index to that of the chosen starting player
      initialGameState = execState chooseStartingPlayer populatedGameState

      startingPlayer = players initialGameState !! currentIx initialGameState
      startingMessage = "\n=== STARTING PLAYER ===\n" ++ "\nPlayer Name: " ++ pName startingPlayer

      -- Repeat the game's loop until finished
      runGameLoop gameState = do

          -- Perform a single turn
          let (turnInfoMessage, updatedGameState) = runState gameLoopWithHistory gameState

          putStrLn turnInfoMessage

          -- Check if the game has finished
          let remainingPlayers = filter (\p ->
                  not (null (hand p)) ||
                  not (null (faceUp p)) ||
                  not (null (faceDown p))
                ) (players updatedGameState)

          if length remainingPlayers > 1
            then runGameLoop updatedGameState
            else do
              let (summaryMessage, _) = runState gameLoopWithHistory updatedGameState
              putStrLn summaryMessage

  putStrLn startingMessage
  runGameLoop initialGameState

--------------------------------------------------------------------------------
-- Step 5 — Smart Player and Tournaments
--------------------------------------------------------------------------------

smartStrategy :: State GameState Deck
smartStrategy = do
  gameState <- get

  let currentPlayer = players gameState !! currentIx gameState
      numberOfPlayers = length (players gameState)
      currentDirection = directionOfPlay gameState
      reversedDirection = negate (directionOfPlay gameState)

      numberOfPlayerCards p = length (hand p) + length (faceUp p) + length (faceDown p)

      stepIx ix direction = (ix + direction + numberOfPlayers) `mod` numberOfPlayers

      findNextActivePlayer ix direction
        | pId (players gameState !! ix) `notElem` finishedOrder gameState = ix
        | otherwise = findNextActivePlayer (stepIx ix direction) direction

      nextPlayerId = findNextActivePlayer (stepIx (currentIx gameState) currentDirection) currentDirection
      reversedNextPlayerId = findNextActivePlayer (stepIx (currentIx gameState) reversedDirection) reversedDirection

      currentHand = sortOn rank (hand currentPlayer)
      currentFaceUp = sortOn rank (faceUp currentPlayer)
      currentFaceDown = faceDown currentPlayer

      effectiveTopCard = find (\card -> rank card /= R8) (discardPile gameState)

      legalHandCards = validPlays effectiveTopCard currentHand
      legalFaceUpCards = validPlays effectiveTopCard currentFaceUp

      -- Decide if playing a 2 is beneficial based on the rank of the top card
      smartPlay2 :: [Card] -> Bool
      smartPlay2 listOfCards =
        any (\card -> rank card == R2) listOfCards &&
        any (\card -> rank card /= R2) listOfCards &&
        case effectiveTopCard of
          Nothing -> False
          Just topCard -> rank topCard >= rankToBeginPlayingR2
        where rankToBeginPlayingR2 = R9

      -- Decide if playing a reversing 8 hinders the player with the least cards
      smartPlay8 :: [Card] -> Bool
      smartPlay8 legalPlays
        | not (containsR8AndOther legalPlays) = False
        | ExtReverse8 `notElem` extensions gameState = False
        | otherwise =
          numberOfPlayerCards (players gameState !! reversedNextPlayerId) >
          numberOfPlayerCards (players gameState !! nextPlayerId)
        where containsR8AndOther listOfCards =
                any (\card -> rank card == R8) listOfCards &&
                any (\card -> rank card /= R8) listOfCards

      -- Decide if playing a 10 is beneficial based on the size of the discard pile
      smartPlay10 :: [Card] -> Bool
      smartPlay10 listOfCards =
        any (\card -> rank card == R10) listOfCards &&
        any (\card -> rank card /= R10) listOfCards &&
        not (null (discardPile gameState)) &&
        length (discardPile gameState) >= sizeToBeginPlaying10
        where sizeToBeginPlaying10 = 8

      -- Choose cards to play using defined heuristics
      smartChooseCards :: [Card] -> [Card]
      smartChooseCards [] = []
      smartChooseCards cards =
        let legalCards = validPlays effectiveTopCard cards
        in
          if smartPlay8 legalCards -- Play all 8s if it hinders the player with least cards
            then filter (\card -> rank card == R8) cards
          else if smartPlay10 legalCards && -- Play all 10s if it results in an empty hand/face-up
              (length cards == length (filter (\card -> rank card == R10) cards))
            then filter (\card -> rank card == R10) cards
          else if smartPlay10 legalCards -- Play a single 10 otherwise
            then [head (filter (\card -> rank card == R10) cards)]
          else if smartPlay2 legalCards && -- Play all 2s if it results in an empty hand/face-up
              (length cards == length (filter (\card -> rank card == R2) cards))
            then filter (\card -> rank card == R2) cards
          else if smartPlay2 legalCards -- Play a single 2 otherwise
            then [head (filter (\card -> rank card == R2) cards)]
          else
            let firstCard = head legalCards
            in firstCard : takeWhile (\c -> rank c == rank firstCard) (tail legalCards)

  if not (null currentHand) then
    case legalHandCards of
      [] -> return []
      exists ->
        let cards = currentHand
        in return (smartChooseCards cards)

  else if not (null currentFaceUp) && null (drawPile gameState) then
    case legalFaceUpCards of
      [] -> return []
      exists ->
        let cards = currentFaceUp
        in return (smartChooseCards cards)

  else if not (null currentFaceDown) then do
    let faceDownCards = currentFaceDown
        (randomIx, generator') = randomR (0, length faceDownCards - 1) (rng gameState)
        card = faceDownCards !! randomIx
    put gameState { rng = generator' }
    return ([card | card `elem` validPlays effectiveTopCard [card]])

  else error "ERROR: An error occurred when attempting to choose a card to play."

-- I have included heuristics that depend on extensions, you may enable them with this line and others:
-- playTournament :: [Extension] -> Int -> IO [(String, Int)]
playTournament :: Int -> IO [(String, Int)]
playTournament numberOfGames = do -- playTournament ext numberOfGames = do

  let gameSeeds = take numberOfGames [1..]

  winningStrategies <- mapM (\gameSeed -> do

      -- Initialise a GameState and its properties
      let generator = mkStdGen gameSeed

          -- Create the draw pile from which to play
          initialDrawPile = shuffleDeck generator [ Card rank suit |
              suit <- [Hearts, Spades, Diamonds, Clubs],
              rank <- [R2 .. RA]
            ]

          -- Initialise the players
          unpopulatedPlayers = [
              Player 1 "BasicPlayer" Basic [] [] [],
              Player 2 "BasicSetsPlayer" BasicSets [] [] [],
              Player 3 "SmartPlayer" Smart [] [] []
            ]

          -- Initialise a game state wherein players have yet to possess cards
          unpopulatedGameState = GameState {
              players = unpopulatedPlayers,
              currentIx = 0,
              drawPile = initialDrawPile,
              discardPile = [],
              burnedPiles = [],
              turnNumber = 1,
              rng = generator,
              finishedOrder = [],
              extensions = [], -- extensions = ext,
              directionOfPlay = 1
            }

          -- Deal three cards to each player's hand, face-up, and face-down list
          populatedGameState = execState (do
              gameState <- get

              initialPlayers <- mapM (\p -> do
                  handCards <- dealCards 3
                  faceUpCards <- dealCards 3
                  faceDownCards <- dealCards 3
                  return p { hand = handCards, faceUp = faceUpCards, faceDown = faceDownCards }
                ) (players gameState)

              gameState <- get

              put gameState { players = initialPlayers }

            ) unpopulatedGameState

          -- Assign the game state's current index to that of the chosen starting player
          initialGameState = execState chooseStartingPlayer populatedGameState

          -- Retrieve the resulting state after game loop has completed
          (_, resultantState) = runState gameLoop initialGameState

      if turnNumber resultantState > maxTurns
        then return Nothing
        else do
          let winningPlayerId =
                case finishedOrder resultantState of
                  (i:_) -> i
                  [] -> error "No winner is present in the game's finished order."

              winningPlayer =
                case find (\p -> pId p == winningPlayerId) (players resultantState) of
                  Just p -> p
                  Nothing -> error "An error occurred when attempting to retrieve the winning player."

          return (Just (strategy winningPlayer))
    ) gameSeeds

  let actualWinningStrategies = [ strategy | Just strategy <- winningStrategies ]
      basicWins = length (filter (== Basic) actualWinningStrategies)
      basicSetsWins = length (filter (== BasicSets) actualWinningStrategies)
      smartWins = length (filter (== Smart) actualWinningStrategies)
      numberOfCompletedGames = length actualWinningStrategies

      -- Create a message detailing the number of games each strategy won
      strategyMessage = [
          ("Basic Strategy: ", basicWins),
          ("Basic Sets Strategy: ", basicSetsWins),
          ("Smart Strategy: ", smartWins),
          ("Total Games Ran to Completion: ", numberOfCompletedGames)
        ]

  return strategyMessage
