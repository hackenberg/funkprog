import Movie

db = [ ("Title1","Regisseur1",["Actor1"],2012,Thriller,10)
     , ("Title1","Regisseur1",["Actor1"],2012,Thriller,10)
     , ("Title1","Regisseur1",["Actor1","Actor2"],2012,Thriller,10)
     , ("Title1","Regisseur1",["Actor2","Actor1"],2012,Thriller,10)
     ]

main = do
    print $ rmDup db
