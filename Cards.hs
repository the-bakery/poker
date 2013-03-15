
module Cards where


data Card = Rank `Of` Suit
          deriving Eq

instance Show Card where

    show (rank `Of` suit) =
        show rank ++ show suit


data Suit = Hearts | Diamonds | Clubs | Spades
          deriving Eq

instance Show Suit where

    show suit =
        case suit of
          Hearts -> "H"
          Diamonds -> "D"
          Clubs -> "C"
          Spades -> "S"


data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace
          deriving (Eq, Ord, Enum, Bounded)


instance Show Rank where

    show rank =
        let value = (fromEnum rank) - (fromEnum Two) + 2
        in
          if value < 11 then show value
          else
              case rank of
                Jack -> "J"
                Queen -> "Q"
                King -> "K"
                Ace -> "A"
