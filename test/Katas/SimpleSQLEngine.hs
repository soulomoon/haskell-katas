module Katas.SimpleSQLEngine (spec) where

import Kyu2.SimpleSQLEngine (sqlEngine)
import Test.Hspec
import Data.List (sort)

movieDatabase = [ ( "movie"
                  , [ [ ( "id", "1" ), ( "name", "Avatar"   ), ( "directorID", "1" ) ]
                    , [ ( "id", "2" ), ( "name", "Titanic"  ), ( "directorID", "1" ) ]
                    , [ ( "id", "3" ), ( "name", "Infamous" ), ( "directorID", "2" ) ]
                    , [ ( "id", "4" ), ( "name", "Skyfall"  ), ( "directorID", "3" ) ]
                    , [ ( "id", "5" ), ( "name", "Aliens"   ), ( "directorID", "1" ) ]
                    ]
                  )
                , ( "actor"
                  , [ [ ( "id", "1" ), ( "name", "Leonardo DiCaprio" ) ]
                    , [ ( "id", "2" ), ( "name", "Sigourney Weaver"  ) ]
                    , [ ( "id", "3" ), ( "name", "Daniel Craig"      ) ]
                    ]
                  )
                , ( "director"
                  , [ [ ( "id", "1" ), ( "name", "James Cameron"   ) ]
                    , [ ( "id", "2" ), ( "name", "Douglas McGrath" ) ]
                    , [ ( "id", "3" ), ( "name", "Sam Mendes"      ) ]
                    ]
                  )
                , ( "actor_to_movie"
                  , [ [ ( "movieID", "1" ), ( "actorID", "2" ) ]
                    , [ ( "movieID", "2" ), ( "actorID", "1" ) ]
                    , [ ( "movieID", "3" ), ( "actorID", "2" ) ]
                    , [ ( "movieID", "3" ), ( "actorID", "3" ) ]
                    , [ ( "movieID", "4" ), ( "actorID", "3" ) ]
                    , [ ( "movieID", "5" ), ( "actorID", "2" ) ]
                    ]
                  )
                ]

spec = do
    describe "execution" $ do
      let execute = sqlEngine movieDatabase
      it "should SELECT columns" $ do
        sort (execute "select movie.name from movie")
          `shouldBe` [ [ ( "movie.name", "Aliens"   ) ]
                     , [ ( "movie.name", "Avatar"   ) ]
                     , [ ( "movie.name", "Infamous" ) ]
                     , [ ( "movie.name", "Skyfall"  ) ]
                     , [ ( "movie.name", "Titanic"  ) ]
                     ]

      it "should apply WHERE" $ do
        sort (execute "SELECT movie.name FROM movie WHERE movie.directorID = '1'")
          `shouldBe` [ [ ( "movie.name", "Aliens"  ) ]
                     , [ ( "movie.name", "Avatar"  ) ]
                     , [ ( "movie.name", "Titanic" ) ]
                     ]
      it "should perform parent->child JOIN" $ do
        sort (execute "Select movie.name, director.name From movie Join director On director.id = movie.directorID")
          `shouldBe` [ [ ( "movie.name", "Aliens"   ), ( "director.name", "James Cameron"   ) ]
                     , [ ( "movie.name", "Avatar"   ), ( "director.name", "James Cameron"   ) ]
                     , [ ( "movie.name", "Infamous" ), ( "director.name", "Douglas McGrath" ) ]
                     , [ ( "movie.name", "Skyfall"  ), ( "director.name", "Sam Mendes"      ) ]
                     , [ ( "movie.name", "Titanic"  ), ( "director.name", "James Cameron"   ) ]
                     ]
      it "should perform child->parent JOIN" $ do
        sort (execute "SelecT movie.name,director.name\nFroM director\nJoiN movie ON director.id = movie.directorID\n")
          `shouldBe` [ [ ( "movie.name", "Aliens"   ), ( "director.name", "James Cameron"   ) ]
                     , [ ( "movie.name", "Avatar"   ), ( "director.name", "James Cameron"   ) ]
                     , [ ( "movie.name", "Infamous" ), ( "director.name", "Douglas McGrath" ) ]
                     , [ ( "movie.name", "Skyfall"  ), ( "director.name", "Sam Mendes"      ) ]
                     , [ ( "movie.name", "Titanic"  ), ( "director.name", "James Cameron"   ) ]
                     ]
      it "should perform many-to-many JOIN and apply WHERE" $ do
        sort (execute "select movie.name   \
                      \     , actor.name\
                      \  FROM movie\
                      \  Join actor_to_movie\
                      \       oN actor_to_movie.movieID=movie.id\
                      \  JoIn actor\
                      \    oN actor_to_movie.actorID = actor.id\
                      \ WheRe \n\
                      \   actor.name <> 'Daniel Craig'")
          `shouldBe` [ [ ( "movie.name", "Aliens"   ), ( "actor.name", "Sigourney Weaver"  ) ]
                     , [ ( "movie.name", "Avatar"   ), ( "actor.name", "Sigourney Weaver"  ) ]
                     , [ ( "movie.name", "Infamous" ), ( "actor.name", "Sigourney Weaver"  ) ]
                     , [ ( "movie.name", "Titanic"  ), ( "actor.name", "Leonardo DiCaprio" ) ]
                     ]
