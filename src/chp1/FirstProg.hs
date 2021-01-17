main = do
  print "Who is the email for?"
  recipient <- getLine
  print "What is the title?"
  title <- getLine
  print "Who is the Auther?"
  auther <- getLine
--   print ("Dear " ++ recipient ++ ",\n" ++ "Thanks for buying" ++ title ++ "\nthanks," ++ auther)
  print (createEmail recipient title auther)

toPart recipient = "Dear " ++ recipient ++ ",\n"

bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ".\n"

fromPart auther = "Thanks,\n" ++ auther

createEmail recipient bookTitle auther=
  toPart recipient++bodyPart bookTitle++fromPart auther