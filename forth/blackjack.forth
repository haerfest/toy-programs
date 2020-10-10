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
  ." Shuffling the deck..." cr
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

\ Returns a hand's points.
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

\ Returns whether a player busted, i.e. passed twenty-one.
: busted? ( addr -- f ) points 21 > ;

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

variable chips

\ Prepares a new game.
: new-game ( -- )
  shuffle-deck
  52 remaining !
  0 player !
  0 dealer !
;

: deal-initial-cards ( -- )
  ." The dealer deals you the two top cards." cr
  draw-card player store-card
  draw-card player store-card

  ." The dealer draws one card." cr
  draw-card dealer store-card

  ." The dealer's hand (" dealer .points ." points):" cr
  dealer .hand
;

\ Plays for the dealer.
: dealer-plays ( -- )
  \ The dealer must stay once at 17 or higher.
  begin
    ." The dealer's hand (" dealer .points ." points):" cr
    dealer .hand
    dealer points 17 <
  while
    ." The dealer draws a card." cr
    draw-card dealer store-card
  repeat

  \ Print the dealer's final points.
  dealer busted?     if ." The dealer busts."          cr else
  dealer blackjack?  if ." The dealer has blackjack."  cr else
  dealer points 21 = if ." The dealer has twenty-one." cr else
                        ." The dealer stays at " dealer .points ." points." cr
  then then then
;

\ Lets the dealer attempt to reach a stand-off.
: dealer-try-stand-off ( -- )
  ." The dealer is given the chance to reach a stand-off." cr
  ." The dealer draws a card." cr
  draw-card dealer store-card
  ." The dealer's hand (" dealer .points ." points):" cr
  dealer .hand
;

\ Let's the player play.
: player-plays ( -- )
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

  \ Print the player's final points.
  player busted?     if ." You bust."            cr else
  player blackjack?  if ." You have blackjack."  cr else
  player points 21 = if ." You have twenty-one." cr else
                        ." You stay at " player .points ." points." cr
  then then then
;

\ Evaluates a finished round. The player's bet is on the stack. Returns
\ what the player wins.
: evaluate-round ( u -- u )
  player busted?                          if ." The dealer wins this round." drop 0   cr else
  player blackjack? dealer blackjack? and if ." It's a stand-off."                    cr else
  player blackjack?                       if ." You win this round."         25 10 */ cr else
  dealer busted?                          if ." You win this round."         2 *      cr else
  player points dealer points =           if ." It's a push."                         cr else
  player points dealer points >           if ." You win this round."         2 *      cr else
                                             ." The dealer wins this round." drop 0   cr
  then then then then then then
;

: ace-or-ten-value? ( addr -- )
  points
  dup  10 =
  swap 11 =
  or
;

\ Plays a game of blackjack.
: play-game ( u -- )
  chips @ over - chips !
  new-game
  deal-initial-cards
  player-plays

  player blackjack? dealer ace-or-ten-value? and if dealer-try-stand-off else
  player blackjack? player busted? or 0=         if dealer-plays
  then then

  evaluate-round
  chips +!
;

\ Asks the user to enter a bet.
: enter-bet ( -- 0 | false | u true )
  cr
  ." You have " chips ? ." chips left. " 
  chips @ if
     begin
       ." Your bet? (0 to quit) "
       tib 3 accept cr  ( u )
       dup if
         >r 0. tib r> >number   ( d addr u )
         if
           \ Entered something invalid.
           ." That is not a valid bet." cr
           drop 2drop
           false
         else
           2drop  \ Drop address and high byte of 0.
           dup 0 chips @ 1+ within if
             true
           else  \ Entered something outside [0, chips] range.
             ." You cannot bet that amount." cr
             false nip
           then
         then
       else
         \ Nothing entered.
         false nip
       then
     until
  else
    \ No more chips left.
    false
  then

  \ If the bet is positive, indicate by pushing true.
  dup if true then
;

: blackjack ( -- )
  begin
    enter-bet
  while
    play-game
  repeat
;

\ Initializes the game.
: init-blackjack ( -- )
  utime drop seed !
  prepare-deck
  100 chips !
;

init-blackjack
