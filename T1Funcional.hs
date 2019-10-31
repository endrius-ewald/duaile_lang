module T1Funcional where

--import Test.QuickCheck
import Control.Monad
import MyStore



--TODO???: Printar a memoria no inicio, printar a memória no final


data Arit = Num Integer | Var String | Mais Arit Arit | Menos Arit Arit | Vezes Arit Arit | Divide Arit Arit

data EBool = B Bool | Not EBool | Iqual Arit Arit | Diff Arit Arit | Minor Arit Arit | Major Arit Arit | MinorIqual Arit Arit | MajorIqual Arit Arit 
 
data Prog = Seqq Prog Prog | Ife EBool Prog Prog | Uaile EBool Prog | Duaile Prog EBool | Foooor Prog EBool Prog Prog | Atrib String Arit | Neutro 


--readAccess
evalArit::Arit -> Store -> Integer
evalArit (Num n) s = n
evalArit (Var c) s = value s c
evalArit (Mais a1 a2) s = (evalArit a1 s) + (evalArit a2 s)
evalArit (Menos a1 a2) s = (evalArit a1 s) - (evalArit a2 s)
evalArit (Vezes a1 a2) s = (evalArit a1 s) * (evalArit a2 s)
evalArit (Divide a1 a2) s = (evalArit a1 s) `div` (evalArit a2 s)


--readAccess
evalEBool::EBool -> Store -> Bool
evalEBool (B b) s = b
evalEBool (Not e1) s = not (evalEBool e1 s)
evalEBool (Iqual a1 a2) s = (evalArit a1 s) == (evalArit a2 s)
evalEBool (Diff a1 a2) s  = (evalArit a1 s) /= (evalArit a2 s)
evalEBool (Minor a1 a2) s = (evalArit a1 s) < (evalArit a2 s)
evalEBool (Major a1 a2) s = (evalArit a1 s) > (evalArit a2 s)
evalEBool (MinorIqual a1 a2) s = (evalArit a1 s) <= (evalArit a2 s)
evalEBool (MajorIqual a1 a2) s = (evalArit a1 s) >= (evalArit a2 s)


--writeAccess
evalProg::Prog -> Store -> Store 

evalProg (Neutro) s = s

evalProg (Atrib c a1) s = update s c (evalArit a1 s)

--evalProg (Seqq p1 p2) s = (evalProg p2 s) . (evalProg p1 s)
evalProg (Seqq p1 p2) s = evalProg p2 (evalProg p1 s)

evalProg (Ife e1 p1 p2) s 
                    | evalEBool e1 s = evalProg p1 s
                    | otherwise = evalProg p2 s

evalProg (Uaile e1 p1) s 
                    | evalEBool e1 s = evalProg (Uaile e1 p1) (evalProg p1 s)
                    | otherwise = s


evalProg (Duaile p1 e1) s
                    | evalEBool e1 sto = evalProg (Duaile p1 e1) sto
                    | otherwise = sto
                where sto = evalProg p1 s



{-
--Foooor Prog1 EBool Prog2 Prog3
--Prog1 cria var, Ebool parada, Prog2 att var, Prog3 corpo for 
--evalProg (Foooor p1 e1 p2 pr) s = let sto = evalProg p1 s in
evalProg (Foooor p1 e1 p2 pr) s = let sto = evalProg p1 s 
                                    in if evalEBool e1 sto 
--                                        then evalProg p2 (evalProg pr sto)
                                        then evalProg (Foooor p1 e1 p2 pr)(evalProg p2 (evalProg pr sto))
                                        else s
--                    | evalEBool e1 s = evalProg p2 (evalProg pr sto)
--                    | otherwise = s
-}


--show (evalProg myFat memoria1 )
--EXEMPLO DE PROGRAMA
-- prex = Seq init loop
-- init =
-- loop = Waile cond corpo

--fatorial
--fat(n)
{-
fat = 1;
while n>0 {
    fat = fat * n;
    n -= 1
}
return fat;
-}

memoria1 = update initial "n" 10

initFat :: Prog
initFat = Atrib ("fat")(Num 1)

loopFat :: Prog
loopFat = Uaile(Major (Var "n")(Num 0))( Seqq (Atrib ("fat")(Vezes (Var "fat")(Var "n")) ) (Atrib ("n")(Menos (Var "n")(Num 1))) )

myFat :: Prog
--myFat = Seqq (initFat)(loopFat) initial
myFat = Seqq initFat loopFat


--multiplicação usando soma como funcao auxiliar
--mult(x,y)
{-
acc = 0;
while(y > 0){
    acc += x;
    y -= 1;
}
return acc;
-}

memoria2 = update initial "x" 89
memoria22= update memoria2 "y" 65

initMult :: Prog
initMult = Atrib ("acc")(Num 0)

loopMult :: Prog
loopMult = Uaile(Major (Var "y")(Num 0))(Seqq( Atrib ("acc")(Mais (Var "acc")(Var "x")))(Atrib ("y")(Menos (Var "y")(Num 1))))

myMult :: Prog
myMult = Seqq initMult loopMult


--potencia com expoentes naturais
--pot(n,p)
{-
acc = 1;
while(p > 0){
    acc = n * acc;
    p -= 1;
}
return acc;
-}

memoria3 = update initial "n" 3
memoria33 = update memoria3 "p" 3

initPot :: Prog
initPot = Atrib ("acc")(Num 1)

loopPot :: Prog
loopPot = Uaile(Major (Var "p")(Num 0))(Seqq( Atrib ("acc")(Vezes (Var "n")(Var "acc")))(Atrib ("p")(Menos (Var "p")(Num 1))))

myPot :: Prog
myPot = Seqq initPot loopPot


--divisao inteira
--div(n,d)
{-
acc = 0;
while(n >= d){
    n = n - d;
    acc++;
}
return acc;
-}

memoria4 = update initial "n" 14
memoria44 = update memoria4 "d" 5

initDiv :: Prog
initDiv = Atrib ("acc")(Num 0)

loopDiv :: Prog
loopDiv = Uaile(MajorIqual (Var "n")(Var "d"))(Seqq( Atrib ("n")(Menos (Var "n")(Var "d")))(Atrib ("acc")(Mais (Var "acc")(Num 1))))

myDiv :: Prog
myDiv = Seqq initDiv loopDiv

--maximo divisor comum
--mdc(n1,n2){
--
--}





