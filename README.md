# TicTacToe AI

This is a TicTacToe AI model I created using Standard ML. The user is able to play a game of TicTacToe against the AI, which is trained on 100,000 games. This AI model was created as part of my Functional Programming class.

All the code for the AI model and the TicTacToe game is in the file final.sml, which is in the folder 'TicTacToe_Files'. The other files mainly consist of helper functions that are used within final.sml.

# How To Use

To see the AI model in action, open the folder TicTacToe_Files. Then run these commands in your terminal:

```smlnj```

```CM.make "sources.cm";```

```val mem = Train.train(100000, Dict.empty, Game.start);```

```Controller.go();```
