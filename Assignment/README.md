# Karma Card Game Haskell Implementation

This is my implementation of Karma, the card game, in Haskell. This was done as part of my coursework for the first semester of my second year. This was my first experience using a functional programming language, and it was not easy as first. However, I persevered. This implementation allows bots to play against each other with various rulesets, some of which can be enabled but are disabled by default. These bots utilise different strategies of play.

To run one single game to completion using only basic strategy bots, do:

```runOneGame```


To run one game that keeps track of the turns, cards, and states of each player, do:

```runOneGameWithHistory```


To run one game with various chosen additional rule extensions, do:

```playOneGameStep4 [INSERT EXTENSIONS HERE, SEPARATED BY ONE SPACE]```

(Possible rule extensions include: 'ExtReverse8,' 'ExtThree3s,' and 'ExtNineClubs.'


To run a tournament of games, which produces the number of times each strategy one, do:

```playTournament [INT]```

(Where [INT] is the number of games you want the tournament to have. NOTE: Games sometimes go on indefinitely (until a memory leak occurs), but the results of the tournament show the amount of games actually completed)
