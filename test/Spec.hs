import Test.Hspec
import Lib

main :: IO()
main = hspec $ do
   describe "Aplicar Hierbas a ratones" $ do
      it "aplicar hierbaBuena a cerebro" $ do
         hierbaBuena cerebro `shouldBe` (Raton "Cerebro" 3 0.2 ["brucelosis","sarampion","tuberculosis"])
      it "aplicar hierbaVerde a cerebro con la terminacion `sis`" $ do
         hierbaVerde "sis" cerebro `shouldBe` (Raton "Cerebro" 9 0.2 ["sarampion"]) 
      it "probar noTerminaEn `sis` la palabra Tuberculosis " $ do 
         noTerminaEn "sis" "tuberculosis" `shouldBe` False
      it "aplicar alcachofa a cerebro" $ do
         alcachofa cerebro `shouldBe` (Raton "Cerebro" 9 0.19 ["brucelosis","sarampion","tuberculosis"])
      it "aplicar hierbaDelDiablo a cerebro" $ do
         hierbaDelDiablo cerebro `shouldBe` (Raton "Cerebro" 9 0.1 ["sarampion"])
   
   describe "PUNTO 4" $ do
      it "cantidadIdeal even" $ do
         cantidadIdeal even `shouldBe` 2
      it "cantidadIdeal (>5)" $ do
         cantidadIdeal (>5) `shouldBe` 6
