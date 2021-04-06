head :: Monoid a => [a] -> a
head (x:_) = x
head []    = mempty

example :: [[Int]]
example = []

-- Main.length

-- Q34.1 Recall that in unit 4 we mentioned that Data.Text
-- is strongly preferred over String for working with text
-- data. Refactor this project to use Data.Text instead of
-- String (in both the Main and Palindrome modules).


-- Q34.2 In unit 4, lesson 25, you wrote a program to
-- “glitch” binary images. Revisit that program and pull
-- out all the code specific to glitching images into its
-- own Glitch module.
