
#Here I assign the entire game of hangman to the playHangman() function so it is
#'to call the game and so the flow of code runs smoothly

playHangman <- function() {
  
  # R opening text file and assigning contents to "words" variable
  
  words <- scan("WordsHangMan.txt", what = character())
  
  # Here I use the sample function to choose a random word from the .txt file and
  #' then I turn the random word into a list and use the string split function 
  #' on it to turn the word into separate characters
  
  magic_word <- sample(words, 1)
  word <- as.list(strsplit(magic_word,"")[[1]])
  
  # In this line of code I assign the number of attempts the user gets to answer 
  #'the word, since the length of the word may vary, i decide the number of attempts 
  #'a user gets based on the length of the word plus 4 attempts so it is fair for
  #'the user.
  
  attempts <- length(word) + 4
  
  #Here I assign all of the accepted inputs for the game to the accepted_inputs variable
  
  accepted_inputs <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", 
                       "m", "n", "o","p","q", "r", "s", "t", "u", "v", "w", "x", 
                       "w", "z")
  
  #' In this line of code I generate the blanks for the game, so I find out the 
  #' length of the randomly generated word using the length function and then I 
  #' use the rep() function to repeat the _ value the same amount of times as is 
  #' the length of the word
  #' 
  blanks <- rep("_", length(word))
  
  #" Here I create an two empty vectors that will store the incorrect and correct
  #letters
  
  incorrect <- character()
  correct <- character()
  
  #This is just an introductory message that prints at the start of each game
  
  print(paste("Hello player, Welcome to Hangman |-O->-<  |. Your magic word is", 
              length(word), "characters in length. In this version of hangman you will be allowed", 
              attempts, "incorrect guesses, if you use up all of your guesses , you will lose the game. You may only input one letter at a time or if you're feeling adventurous you may decide to input the entire word in one go."), quote = FALSE)
  print("", quote = FALSE)
  
  
  # Game loop
  #' Here I am creating a loop that starts by checking if the number of blanks is
  #' greater that 0 and if the number of remaining attempts is also greater than 0.
  #' While these conditions are true, messages displaying the blanks remaining, 
  #' incorrectly guessed letters, and number of attempts remaining are printed. A
  #' prompt is also printed asking the user to enter a letter and the to lower() function
  #' is used to change the input to lowercase.  
  
  while (sum(blanks == "_") > 0 && attempts > 0) {
    cat("Current word:", paste(blanks, collapse = " "), "\n")
    cat("Incorrect guesses:", paste(incorrect, collapse = " "), "\n")
    cat("Attempts remaining:", attempts, "\n")
    guess <- readline("Please enter your guess (single letter or the entire word): ")
    guess <- tolower(guess)
    
    #'An if statement is added to check if the input is equal to the randomly generated 
    #'word, if it is, thee blanks are replaced with the word and the loop is escaped. 
    #'If the guess is not equal to the magic word,the next if statement is considered.
    
    if (guess == magic_word) {
      blanks <- word
      break
      
      #'The next if statement checks to make sure that the input was 1 letter long,
      #'and that the input is apart of the accepted_inputs list created above. 
      #'If both conditions are satisfied we move on to the next if statement checks to see if 
      #'the input letter is found in the magic word, 
      #'
    } else if (nchar(guess) == 1 && guess %in% accepted_inputs) {
      
      #checks to see if the input letter is found in the magic word, if the letter inputted is correct
      #'the code then checks if the letter was already guessed previously and if it was correct, if not
      #'then the letter replaces one of the blanks and is added to the list of 
      #'correct guesses. 
      
      if (guess %in% word) {
        if (guess %in% correct) {
          cat("You have already guessed this letter correctly. Try again.\n")
        } else {
          blanks[word == guess] <- guess
          correct <- c(correct, guess)
        }
        
        #'If the previous if statement is not satisfied, the code checks to see if the letter
        #'entered is either present in the incorrect vector or correct vector, if it is 
        #'then a message is printed, if the input is not present in either of these then 
        #'it is added to the incorrect vector and the number of available attempts is subtracted by 1.
        #'If none of those conditions are satisfied a message saying the input was invalid
        #'is printed since at this point the user must have enter a character that is not
        #'considered a letter.
        
      } else {
        # Incorrect guess
        if (guess %in% incorrect || guess %in% correct) {
          cat("You have already guessed this letter. Try again.\n")
        } else {
          incorrect <- c(incorrect, guess)
          attempts <- attempts - 1
        }
      }
    } else {
      cat("Invalid input. Please enter a single letter or the entire word.\n")
    }
  }
  
  #To calculate the total number of attempts, the length of the "incorrect" and 
  #"correct" vectors is taken and added together.
  
  total_attempts <- length(incorrect) + length(correct)
  
  #'This code checks to see if the number of remaining blanks is equal to 0, if it 
  #'is this means the user has discovered all of the hidden letters and has completed
  #'the magic word, at this point a congratulations message is displayed, If this is 
  #'not satisfied the final else code line is called and a message saying good game 
  #'is printed and the user loses the round of hangman
  
  cat("Secret word:", magic_word, "\n")
  if (sum(blanks == "_") == 0) {
    cat("CONGRATULATIONS!!! You have guessed the magic word '", paste(word, collapse = ""), "' with ", attempts, " attempts remaining.\n")
  } else {
    cat("Unfortunately, you have not been able to guess the magic word :(. Better luck next time!\n")
  }
}

playHangman()

### Code Review by Ramiz Khan ###

# Very well written script, game runs perfectly and is idiot-proofed for all types of inputs
# I like how I can start the game by running the function
# accepted_inputs variable can be replaced by c("letters","LETTERS") which accounts for all lower case and upper case letters
# When i try to guess the entire word and i get the guess wrong, i do not lose an attempt, maybe adjust an else if statement for guess != magic_word to attempts - 1