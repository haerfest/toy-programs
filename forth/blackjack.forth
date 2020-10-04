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
    dup 8 > if  \ Joker, queen and king are 10.
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

\ Creates a player.
: create-player
  create
    0 ,       \ Number of cards drawn.
    12 allot  \ The drawn cards, 12 cards will certainly pass 21.
    dup
    ,         \ Length of name.
    0 do      \ Name.
      dup i + c@ c,
    loop
    drop
;

\ Returns a player's cards.
: cards ( addr - addr ) 1 cells + ;

\ Prints a player's name.
: .name ( addr -- )
  cards
  12 +
  dup
  1 cells +
  swap
  @
  type
;

\ Stores a card in a player's hand.
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

\ Sets the top of the stack up for a do/loop over a player's cards.
: each-card ( addr -- )
  dup   ( addr addr )
  cards ( addr c-addr )
  swap  ( c-addr addr )
  @     ( c-addr count )
  over  ( c-addr count c-addr )
  +     ( c-addr c-addr+count )
  swap  ( c-addr+count c-addr )
;

\ Returns a player's points. TODO: Deal with ace's dual values.
: points ( addr -- u )
  0 swap
  each-card
  do
    i c@ value +
  loop
;

\ Shows the hand of cards.
: .hand ( addr -- )
  dup
  ." The " .name ." 's hand:" cr
  dup
  each-card
  do
    2 spaces
    i c@ .card
    cr
  loop
  ." Points: " points . cr
;

s" player" create-player player
s" dealer" create-player dealer

\ Resets the player's and banker's points.
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
  dealer .hand

  begin
    player .hand

    true
    player points 21 < if
      ." Another card? "
      key cr
      [char] y =
      if
        draw-card player store-card
        false nip
      then
    then
  until

  ." The " player .name space
  player points 21 > if
    ." busts."
  else
    player points 21 = if
      ." has twenty-one."
    else
      ." stands at " player points [char] . emit
    then
  then
  cr
;
