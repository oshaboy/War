import System.Random
import Control.Concurrent
import Text.Printf
import Debug.Trace
import System.Exit
import Control.Monad.State
shuffle :: [a] -> StdGen -> (StdGen, [a])
shuffle list generator' = 
    let len=length list in 
    case len of 
        0 -> (generator', [])
        1 -> (generator', list)
        _ ->
            let old_initial=head list in 
            let (num, newgen') = uniformR (0 :: Int, (len - 1)) generator' in 
            
            let (newgen,residue ) = shuffle (tail list) newgen' in 
            if num==0 then (newgen, old_initial:residue)
            else let (before, after') = splitAt (num-1) residue in
            let new_initial=head after' in
            let after = tail after' in
            (newgen, [new_initial]++before++[old_initial]++after)

data Suit = Clubs | Diamonds | Hearts | Spades | Red | Black | Green deriving (Eq) 
data Rank =  Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace | Joker deriving (Eq,Ord)
data Card = Card {r :: Rank, s :: Suit} 
showsuit :: Suit -> Char
showrank :: Rank -> Char
instance Show Suit where show s = [showsuit s]
instance Show Rank where show r = [showrank r]
instance Show Card where show (Card r s) = show r ++ show s
instance Eq Card where (==) c1 c2 = (r c1) == (r c2)
instance Ord Card where
    (<=) c1 c2 = (r c1) <= (r c2)
    (<) c1 c2 = (r c1) < (r c2)
    (>) c1 c2 = (r c1) > (r c2)
allSimpleSuits :: [Suit]
allSimpleRanks :: [Rank]
type Deck = [Card]
unshuffled_deck :: Deck
data Result = Player1_Win | Player2_Win | Timeout | Endless  deriving (Eq)

data GameState = GameState {
    player1_deck :: Deck,
    player2_deck :: Deck,
    player1_discard :: Deck,
    player2_discard :: Deck,
    move_count :: Int,
    iom :: IO (),
    generator :: StdGen
}
data GameResult = GameOver (Result, Int, IO ()) | GameNotOver
type GameStateState = State GameState GameResult
game :: GameStateState
fight :: GameStateState
gamerec ::  GameStateState
initialize_game_state :: StdGen -> GameState 
print_battlers :: [Card] -> [Card] -> GameStateState



allSimpleRanks = [Ace, Two , Three , Four , Five , Six , Seven , Eight , Nine , Ten , Jack , Queen , King]
allSimpleSuits=  [ Clubs , Diamonds , Hearts , Spades ]
showrank r = case r of 
    Ace -> 'A' 
    Two -> '2'
    Three -> '3'
    Four -> '4'
    Five -> '5'
    Six -> '6'
    Seven -> '7' 
    Eight -> '8'
    Nine -> '9'
    Ten -> 'X'
    Jack -> 'J'
    Queen -> 'Q'
    King-> 'K'
    Joker -> '*'
showsuit s = case s of
    Clubs -> '♣'
    Spades -> '♠'
    Diamonds -> '♦'
    Hearts -> '♥'
    Red -> 'R'
    Black -> 'B'
unshuffled_deck = 
    (do 
    simple_suit <- allSimpleSuits
    simple_rank <- allSimpleRanks
    return (Card simple_rank simple_suit))
    ++
    (do
    joker_suit <- [Red , Black]
    return (Card Joker joker_suit))


fight = do
    game_state <- get
    let (p1d,p1ds,newgen') = if ((length $ player1_deck game_state)==0)
        then 
            let p1d' = player1_discard game_state in 
            let (newgen'', p1d'') = shuffle p1d' (generator game_state) in
            (p1d'', [], newgen'')
        else (player1_deck game_state, player1_discard game_state,generator game_state) in
        let (p2d,p2ds,newgen) = if ((length $ player2_deck game_state)==0)
            then 
                let p2d' = player2_discard game_state in 
                let (newgen'',p2d'') = shuffle p2d' newgen' in
                (p2d'', [], newgen'')
            else  (player2_deck game_state, player2_discard game_state,newgen') in 
            let p1b=[head p1d ] in
            let p2b=[head p2d ] in
        if ((last p1b) > (last p2b)) then do
            game_state <- get
            put GameState {
                player1_deck = tail p1d,
                player2_deck = tail p2d,
                player1_discard =p1ds ++ p1b ++ p2b,
                player2_discard= p2ds,
                move_count= move_count game_state+1,
                iom = iom game_state,
                generator=newgen 
            }
            print_battlers p1b p2b
        else if ((last p1b) < (last p2b)) then do
            game_state <- get
            put GameState {
                player1_deck = tail p1d,
                player2_deck = tail p2d,
                player1_discard = p1ds ,
                player2_discard= p2ds ++ p1b ++ p2b,
                move_count= move_count game_state+1,
                iom = iom game_state,
                generator=newgen
            }
            print_battlers p1b p2b

        else {-((last p1b) == (last p2b))-}  do

            game_state <- get
            put GameState {
                player1_deck = tail p1d,
                player2_deck = tail p2d,
                player1_discard = p1ds,
                player2_discard = p2ds,
                move_count= move_count game_state+1,
                iom = iom game_state,
                generator=newgen 
            } 
            print_battlers p1b p2b
            game_state <- get
            let war iom' p1d p2d p1b p2b =
                    let n = 3 in
                    let (newbattlers_p1', newdeck_p1) = splitAt n p1d in
                    let (newbattlers_p2', newdeck_p2) = splitAt n p2d in 
                    let newbattlers_p1 = p1b ++ newbattlers_p1' in
                    let newbattlers_p2 = p2b ++ newbattlers_p2' in
                    if ((last newbattlers_p1) > (last newbattlers_p2)) then do
                        game_state <- get
                        put GameState {
                            player1_deck = newdeck_p1,
                            player2_deck = newdeck_p2,
                            player1_discard = player1_discard game_state ++ newbattlers_p1 ++ newbattlers_p2,
                            player2_discard = player2_discard game_state,
                            move_count= move_count game_state+1,
                            iom = iom',
                            generator=newgen
                        } 
                        print_battlers  newbattlers_p1 newbattlers_p2
                        return GameNotOver
                    else if ((last newbattlers_p1) < (last newbattlers_p2)) then do
                        game_state <- get
                        put GameState {
                            player1_deck = newdeck_p1,
                            player2_deck = newdeck_p2,
                            player1_discard = player1_discard game_state,
                            player2_discard = player2_discard game_state  ++ newbattlers_p2 ++ newbattlers_p1,
                            move_count= move_count game_state+1,
                            iom = iom',
                            generator=newgen
                        } 
                        print_battlers  newbattlers_p1 newbattlers_p2
                        return GameNotOver
                    else 
                        if (length newdeck_p1 == 0) || (length newdeck_p2 == 0) then do
                            game_state <- get
                            return $ GameOver (Endless, move_count game_state+1, iom')  
                        else
                            war (iom') newdeck_p1 newdeck_p2 newbattlers_p1 newbattlers_p2
                        in war (iom game_state) (tail p1d) (tail p2d) p1b p2b 
    

    
gamerec = 
    do
        game_state <- get
        if move_count game_state >= 50000 then
            return $ GameOver (Timeout,move_count game_state, iom game_state)
        else if ((((length (player1_deck game_state))/=0) || ((length (player1_discard game_state))/=0)) &&
            (((length (player2_deck game_state))/=0) || ((length (player2_discard game_state))/=0))) then do
            isover <- fight  
            case isover of
                GameNotOver -> gamerec
                GameOver x -> return $ GameOver x
        else do
            let iom' = iom game_state in
                if ((length (player2_deck game_state))==0) then
                    return $ GameOver (Player1_Win,move_count game_state,iom')
                else if ((length (player1_deck game_state))==0) then 
                    return $ GameOver (Player2_Win,move_count game_state,iom')
                else undefined 
    
game = 
    gamerec 
initialize_game_state generator' = 
    let deck_size = length unshuffled_deck in
    let (newgen,deck) = shuffle unshuffled_deck generator' in
    let half = div deck_size 2 in
    let (p1d,p2d)=splitAt half deck in 
    GameState {
        player1_deck = p1d,
        player2_deck = p2d,
        player1_discard =[],
        player2_discard=[],
        move_count=0,
        iom = putStr "\n\n\n\n\n\n\n\n\n",
        generator=newgen

    }
main = do 
    generator' <- getStdGen
    let result = evalState game (initialize_game_state generator') in
        case result of 
            GameNotOver -> die "Game returned as Not Over\n"
            GameOver (winner,move_count', iom') -> do
                iom'
                case winner of 
                    Player1_Win -> printf "\n\n\nPlayer 1 Wins after %d moves\n" move_count'
                    Player2_Win -> printf "\n\n\nPlayer 2 Wins after %d moves\n" move_count'
                    Timeout -> putStrLn "Timeout"
                    Endless -> putStrLn "Endless"
                

            


print_battlers player1_battlers player2_battlers = do

    game_state <- get 
    let iom' = (do
        putStr (if (length player1_battlers)>2 then "\a" else "")
        printf "\x1b[8A\rmove=%d\n\n\x1b[K%d %d\n\n\x1b[K\t%s\n\x1b[K\t%s\n\n%d %d\x1b[K\n" (move_count game_state) (length $ player1_deck game_state) (length $ player1_discard game_state) (show  player1_battlers ) (show  player2_battlers) (length $ player2_deck game_state) (length $ player2_discard game_state)
        threadDelay 250000
        ) in 
            put GameState {
                player1_deck = player1_deck game_state,
                player2_deck = player2_deck game_state,
                player1_discard =player1_discard game_state,
                player2_discard= player2_discard game_state,
                move_count=move_count game_state,
                iom = iom game_state >> iom',
                generator = generator game_state
            } >> return GameNotOver
