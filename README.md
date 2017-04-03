# Regex-To-FSA
Repository to transform Regex into Finite State Automata and Optimal forms of FSA. The features of Haskell that we are trying to highlight using this project are polymorphism, type classes, higher order functions and modularization. The pattern matching capability of Haskell is the feature that we use the most to match with the correct expression.

 - **Load the file.**
      ```haskell
      ghci> :l regExpToNFA.hs
      ```
 - **Type in the regular expression as shown. (Inside the code itself)** 
     ```haskell
     reg = (a*b)* -> Star (Then (Star (Literal 'a')) (Literal 'b') ) 
     reg = (ab | a*) -> Or ((Then (Literal 'a') (Literal 'b'))  (Star (Literal 'a')) )
     ```
- **Then type as follows.**
     ```haskell
     ghci> printNFA (build reg)
     ``` 
- **Example.**   
     ```haskell
      reg = (a*b)*
     *Main> printNFA (build reg)
      States:
      [0,1,2,3,4,5,6,7,8,9]

       Moves:
       Move 3 'a' 4
       Move 6 'b' 7
       EMove 0 1
       EMove 0 9
       EMove 1 2
       EMove 2 3
       EMove 2 5
       EMove 4 3
       EMove 4 5
       EMove 5 6
       EMove 7 8
       EMove 8 1
       EMove 8 9

       Start State:
       0

       Final States:
       [9]
     ```
