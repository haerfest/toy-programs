\ Cards are numbered from 0 to 51 as follows:
\
\          two three four five six seven eight nine ten jack queen king ace
\    clubs   0     1    2    3   4     5     6    7   8    9    10   11  12
\ diamonds  13    14   15   16  17    18    19   20  21   22    23   24  25
\   hearts  26    27   28   29  30    31    32   33  34   35    36   37  38
\   spades  39    40   41   42  43    44    45   46  47   48    49   50  51

\ Returns the suit of a card.
: suit ( u -- u ) 13 / ;

\ Returns the rank of a card.
: rank ( u -- u ) 13 mod ;

\ Prints the suit of a card.
: "suits" c" clubs   diamondshearts  spades  " ;
: .suit ( u -- ) suit 8 * "suits" 1+ + 8 -trailing type ;

\ Prints the rank of a card.
: "ranks" c" two  threefour five six  seveneightnine ten  jack queenking ace  " ;
: .rank ( u -- ) rank 5 * "ranks" 1+ + 5 -trailing type ;

\ Returns the (maximum) value of a card.
: value ( u -- u )
  rank
  dup 12 = if   \ Highest value of an ace is 11.
    11 nip
  else
    dup 8 > if  \ Jack, queen and king are 10.
      10 nip
    else
      2 +       \ Any other card's value is its rank + 2.
    then
  then
;

\ Prints a card.
: .card ( u -- ) dup .rank ."  of " .suit ;
  
\ Keeps track of our deck of 52 undrawn cards.
variable deck 51 allot

\ Keeps track how many cards remain in our deck.
variable remaining

\ Our current random number seed.
variable seed

\ Generates a random number.
: random ( -- u )
  seed @ 31421 * 6927 +
  dup seed !
;

\ Generates a random number in the range [0, u).
: choose ( u -- u ) random um* nip ;

\ Returns the address of the n'th card, counting from zero.
: card ( u -- addr ) deck + ;

\ Prints the deck.
: .deck ( -- )
  52 0 do
    i card c@ .card cr
  loop
;

\ Resets the deck of cards.
: prepare-deck ( -- )
  52 0 do
    i dup card c!
  loop
  52 remaining !
;

\ Returns whether the deck is empty.
: empty? ( -- f ) remaining @ 0= ;

\ Draws a card from the deck.
: draw-card ( -- u )
  remaining @ 1-
  dup remaining !
  card c@
;

\ Swaps two cards in the deck at indices u1 and u2.
: swap-cards ( u1 u2 -- )
  2dup
  card c@ swap card c@
  rot
  card c! swap card c!
;

\ Shuffles the deck of cards.
: shuffle-deck ( -- )
  1 51 do
    i dup 1+ choose swap-cards
  -1 +loop
;

\ Creates a new (hand for a) player.
: hand
  create
    0 ,       \ Number of cards drawn.
    12 allot  \ The drawn cards, 12 cards will certainly pass 21.
;

\ Returns the address of the cards in a hand.
: cards ( addr - addr ) 1 cells + ;

\ Stores a card in a hand.
: store-card ( u addr -- )
  tuck  ( addr u addr )
  dup   ( addr u addr addr )
  cards ( addr u addr c-addr )
  swap  ( addr u c-addr addr )
  @     ( addr u c-addr count )
  +     ( addr u c-addr+count )
  c!    ( addr )
  1     ( addr 1 )
  swap  ( 1 addr )
  +!
;

\ Returns the number of cards in a hand.
: count ( addr -- ) @ ;

\ Sets the top of the stack up for a do/loop over the cards in a hand,
\ starting with the card at addr and u cards remaining.
: each-card ( addr -- )
  dup count  ( addr count )
  swap       ( count addr )
  cell+      ( count c-addr )
  swap       ( c-addr count )
  over       ( c-addr count c-addr )
  +          ( c-addr c-addr+count )
  swap       ( c-addr+count c-addr )
;

\ Given two point counts u1 and u2, select the one closest to 21,
\ but not going over.
: best-points ( u1 u2 -- u3 )
  over 21 <=  ( u1 u2 u1<=21 )
  over 21 <=  ( u1 u2 u1<=21 u2<=21 )
  and         ( u1 u2 f )
  if
    max       ( u3 )
  else
    min       ( u3 )
  then
;

\ Given the address of a range of cards to consider, and the points
\ u1 so far, examine each card in turn and calculate the total
\ number of points. When encountering an ace, try both its values
\ and figure out which gets us closest to twenty-one without going
\ over.
: (points)  ( high-addr low-addr u1 -- high-addr low-addr u2 )
  -rot      ( u high-addr low-addr )
  2dup      ( u high-addr low-addr high-addr low-addr )
  =         ( u high-addr low-addr f )
  if        ( u high-addr low-addr )
    \ All cards added up, we're done.
    rot     ( high-addr low-addr u )
  else
    \ At least one more card to add up.
    rot           ( high-addr low-addr u )
    swap          ( high-addr u low-addr )
    dup           ( high-addr u low-addr low-addr )
    c@ value      ( high-addr u low-addr value )
    dup 11 < if   ( high-addr u low-addr value )
      \ Not an ace, add up regularly.
      rot         ( high-addr low-addr value u )
      +           ( high-addr low-addr u+value )
      swap        ( high-addr u+value low-addr )
      dup         ( high-addr u+value low-addr low-addr )
      >r          ( high-addr u+value low-addr )
      1+          ( high-addr u+value low-addr+1 )
      swap        ( high-addr low-addr+1 u+value )
      recurse     ( high-addr low-addr+1 u' )
      nip         ( high-addr u' )
      r>          ( high-addr u' low-addr )
      swap        ( high-addr low-addr u' )
    else          ( high-addr u low-addr 11 )
      \ An ace, try values 11 and 1.
      drop        ( high-addr u low-addr )
      dup         ( high-addr u low-addr low-addr )
      >r          ( high-addr u low-addr )
      1+          ( high-addr u low-addr+1 )
      swap        ( high-addr low-addr+1 u )
      dup         ( high-addr low-addr+1 u u )
      >r          ( high-addr low-addr+1 u )
      1+          ( high-addr low-addr+1 u+1 )
      recurse     ( high-addr low-addr+1 u1 )
      -rot        ( u1 high-addr low-addr+1 )
      r>          ( u1 high-addr low-addr+1 u )
      11 +        ( u1 high-addr low-addr+1 u+11 )
      recurse     ( u1 high-addr low-addr+1 u11 )
      >r          ( u1 high-addr low-addr+1 )
      rot         ( high-addr low-addr+1 u1 )
      r>          ( high-addr low-addr+1 u1 u11 )
      best-points ( high-addr low-addr+1 u )
      nip         ( high-addr u )
      r>          ( high-addr u low-addr )
      swap        ( high-addr low-addr u )
    then
  then
;

\ Returns a hand's points. TODO: Deal with ace's dual values.
: points ( addr -- u )
  dup count ( addr count )
  swap      ( count addr )
  cell+     ( count low-addr )
  swap      ( low-addr count )
  over      ( low-addr count low-addr )
  +         ( low-addr high-addr )
  swap 0    ( high-addr low-addr 0 )
  (points)  ( high-addr low-addr u )
  nip nip   ( u )
;

\ Returns whether a player has blackjack (an ace and a ten-value card).
: blackjack? ( addr -- f )
  dup  count   2 =
  swap points 21 =
  and
;

\ Prints a hand's points.
: .points ( addr -- ) points . ;

\ Shows a hand of cards.
: .hand ( addr -- )
  each-card
  do
    2 spaces
    i c@ .card
    cr
  loop
;

hand player
hand dealer

\ Resets the player's and dealer's points.
: reset-points ( -- )
  0 player !
  0 dealer !
;

\ Initializes the game.
: init ( -- )
  here seed !
  prepare-deck
;

init

\ Plays a (simplified) game of blackjack.
: blackjack ( -- )
  cr
  shuffle-deck
  reset-points

  ." The dealer deals you the two top cards." cr
  draw-card player store-card
  draw-card player store-card

  ." The dealer draws one card." cr
  draw-card dealer store-card

  ." The dealer's hand (" dealer .points ." points):" cr
  dealer .hand

  begin
    ." Your hand (" player .points ." points):" cr
    player .hand

    true  \ Assume player is done.
    player points 21 < if
      ." Another card? "
      key cr
      [char] y =
      if
        draw-card player store-card
        false nip  \ Player is not done.
      then
    then
  until

  true  \ Assume the dealer gets to play.
  player blackjack? if
    ." You have blackjack! You win this round." cr
    false nip  \ Dealer does not get to play.
  else
    player points 21 = if
      ." You have twenty-one." cr
    else
      player points 21 > if
        ." You busted, the dealer wins this round." cr
        false nip  \ Dealer does not get to play.
      else
        ." You stay at " player .points ." points." cr
      then
    then
  then

  if
    ." TODO: The dealer must play now." cr
  then
;
