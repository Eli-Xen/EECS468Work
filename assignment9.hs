--Haskel Nim star game 
--inputs: ints 
--outputs: chars displaying the board 
--collaborators: 
--creation date: 5/5/25 

type Board=[[Char]] --board is list of list of chars that will be holding stars 
start::Board --initialize the board 
start=["*****","****","***","**","*"] --initalize all the stars 

nim::IO() --starts the board and game 
nim=play start 1

play::Board->Int->IO() --shows board, checks for win, asks for stars, validates input and removes stars 
play board player = do --takes board and player number 
    putStrLn "" --newline so now crowded 
    printBoard board --prints board 
    if isGameOver board then --check if board is empty 
        putStrLn ("\nplayer " ++ show (otherPlayer player) ++ " won") --if true display winner; show makes an int into a string, dont forget to use special " 
    else do --if board isnt empty do...
        putStrLn("\nplayer " ++ show player ++ ", your turn enter a row number: ")
        playRowInput<-getLine --saves what player entered 
        putStrLn "how many stars * to remove: " --ask how many stars to remove 
        remStars<-getLine --save input to remStars 
        let row=read playRow::Int --convert from string to int, i needed help for this 
        let stars=read remStars::Int --do same for stars input 
        if isValidMove board row stars then do --check that move is valid and if yes then do... 
            let newBoard = removeStars board (row-1) stars --update the board 
            play newBoard (otherPlayer player) --recurse to the next loop 
        else do --invalid move 
            putStrLn "you gave invalid move, row num has to be between 1-5 and you can only remove exisiting stars, try again: " --tell the player that they did wrong 
            play board player --recurse 

-- Print the board row by row with row numbers
printBoard::Board->IO() --takes board and outputs to IO 
printBoard []=return() --if empty return and stop recursion 
printBoard(row:rows)=do --if 2D list do...
    putStrLn row --print the row 
    printBoard rows --recurse to next row 
    
isGameOver::Board->Bool --checks if board is empty 
isGameOver=all null --returns if thing passed is null 

-- Switch players: from 1 to 2 and vice versa
otherPlayer::Int->Int --go from player 1 to 2 and from 2 to 1 depending on whats passed 
otherPlayer 1=2 --if currently player 1 then return 2 
otherPlayer 2=1 --if currently player 2 then return 2 
otherPlayer _=1 --if start of game player 1 starts 

isValidMove::Board->Int->Int->Bool --make sure theres enough stars in the row and row number picked is within bounds 
isValidMove board row stars= --recieves board, row #, and starts # to remove 
    row>=1 && row<=length board && --row is at least 1 and less than the length of the board 
    stars>=1 && stars<=length (board !! (row-1)) --stars is at least 1 and less than length of that specific row in the board 

removeStars :: Board->Int->Int->Board -- delete numStars from a specific row by editing the string 
removeStars board row numStars = --takes the board, which row to remove from, and number of stars to remove 
    take row board ++ --gets row that will be changed 
    [take (length (board !! row)-numStars) (board !! row)] ++ --take length of row and shorten by the number of stars 
    drop (row+1) board --skips other rows

