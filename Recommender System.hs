import System.Random
import System.IO.Unsafe

randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))

users = ["user1","user2" , "user3" , "user4"]
items = [ "item1" , "item2" , "item3" , "item4" , "item5" , "item6" ]
purchasesHistory = [("user1",[["item1","item2","item3"],["item1", "item2" , "item4" ] ] ) ,("user2" , [ [ "item2" , "item5" ] , [ "item4" , "item5" ] ] ),("user3" , [ [ "item3" , "item2"]] ) ,("user4" , [] )]

--------------------------createEmptyFreqList------------------------
createEmptyFreqList :: [a] -> [(a, [b])]

createEmptyFreqList [] = []
createEmptyFreqList (i:items) = (i,[]) : (createEmptyFreqList items)

--------------------------getAllUsersStats-------------------------
getAllUsersStats :: [(String, [[String]])] -> [(String, [(String, [(String, Int)])])]

getAllUsersStats ((u,userItems):xs) = helperStats (createEmptyFreqList items) ((u,userItems):xs)  

userItem (x,list) (u,userItems)= (x,items) where items= itematRemZero (itemat x (distinct userItems) userItems)

allUserItems [] _ = []
allUserItems ((x,list):xs) (u,items) = [userItem (x,list) (u,items)] ++ (allUserItems xs (u,items)) 

userAdd ((x,list):xs) (u,items) = (u,allUserItems ((x,list):xs) (u,items))

helperStats _ [] =[]
helperStats ((x,list):xs) ((u,items):ys) = [userAdd ((x,list):xs) (u,items)] ++ (helperStats ((x,list):xs) ys)



countPair currItem outItem list =(outItem ,count currItem outItem list) 
--getCountPair currItem outItem list = countPair currItem outItem list
--countPairHelp(x,y) = if(y==0) then countPairZero 
					  --else (x,y)

count currItem outItem [] = 0
count currItem outItem (i:items) = if ismemeber currItem i && ismemeber outItem i then 1+(count currItem outItem items) else (count currItem outItem items)  

itemathelper y [] list=[]
itemathelper y (x:xs) list =if(y/=x) then ( countPair y x list): (itemathelper y xs list) else itemathelper y xs list 

itemat curritem itemsDis list =  dis (itemathelper curritem itemsDis list ) 

itematRemZero [] = [] 
itematRemZero ((x,y):xs) = if (y==0) then itematRemZero xs
							else (x,y): (itematRemZero xs)




flatten[]=[]
flatten (x:xs)= x ++ (flatten xs) 

dis :: Eq a => [a] -> [a]
dis [] =[] 
dis (x:xs)|ismemeber x xs=dis xs
		  |otherwise=(x:dis xs)
ismemeber x []=False
ismemeber x (y:ys)|x==y=True
				  |otherwise=ismemeber x ys 

distinct []=[]
distinct list= dis (flatten list)

-------------------------freqListItems--------------------------------
freqListItems:: String -> [(String, Int)]

freqListItems user = (occInUserStatsAllItems items userItems) where userItems = (getItemsOfUserUS user (getAllUsersStats purchasesHistory))

getItemsOfUserUS user ((u,items):xs) = if (user==u) then items else getItemsOfUserUS user xs
                
getBindingsOfItem item ((i,bindings):xs) = if (item == i) then bindings 
                                                                          else getBindingsOfItem item xs

getFreqOfBindedItem _ [] = 0                                                                      
getFreqOfBindedItem item ((i,n):xs) = if(item==i) then n else getFreqOfBindedItem item xs

occInUserStats _ [] =0
occInUserStats item ((i,bindings):xs) = if (item/=i) then (getFreqOfBindedItem item bindings) + (occInUserStats item xs)
                                                                        else (occInUserStats item xs) 

occInUserStatsPair item ((i,bindings):xs)= (item,(occInUserStats item ((i,bindings):xs)))
                        
occInUserStatsAllItems items userItems = itematRemZero (occInUserStatsAllItemsHelp items userItems)

occInUserStatsAllItemsHelp [] _ =[]                     
occInUserStatsAllItemsHelp (i:is) userItems = [occInUserStatsPair i userItems ] ++ (occInUserStatsAllItemsHelp is userItems) 

--------------------------freqListCart------------------------
freqListCart:: String ->[String] -> [(String, Int)]

freqListCart user cart= itemAdderList(trial3 user cart)

trial3 user cart=cartAndItemsIntersection cart (userIntersection user (getAllUsersStats purchasesHistory))


pairGen itemx sarasofiapairs = (itemx, (itemAdder itemx sarasofiapairs) )

--takes list of pairs and returns list of pairs (added)
itemAdder _ []= 0
itemAdder itemx ((item,freq):xs) = if itemx == item then freq+ (itemAdder itemx xs) else (itemAdder itemx xs)

itemfilter item [] = []
itemfilter item ((anItem,freq):xs) = if item /= anItem then (anItem,freq):itemfilter item xs else itemfilter item xs

itemAdderList []=[]
itemAdderList ((itemx,freq):xs) = (pairGen itemx ((itemx,freq):xs)) : (itemAdderList (itemfilter itemx  ((itemx,freq):xs)))

userIntersection _ []=[]
userIntersection a ((b,list):xs)=if(a==b) then list else userIntersection a xs

cartAndItemsIntersection [] _=[]
cartAndItemsIntersection (x:xs) list= goOnList x list ++cartAndItemsIntersection xs list


goOnList _ []=[]
goOnList x ((item,list):ys)=if(x==item) then list else goOnList x ys

removescalans _ []=[]
removescalans item ((i,n):xs)=if(item/=i) then (i,n):removescalans item xs else removescalans item xs

-----------------------freqListCartAndItems----------------------------
freqListCartAndItems:: String -> [String] -> [(String, Int)]

freqListCartAndItems user cart = addfreq (freqListItems user) (freqListCart user cart)

addfreq [] _ = []
addfreq (pair:xs) list= (hassanpair pair list) : (addfreq xs list)

hassanpair (item1,freq1) []= (item1,freq1)
hassanpair (item1,freq1) ((item2,freq2):fs) = if (item1==item2) then (item1,freq1+freq2) else hassanpair (item1,freq1) fs

---------------------------recommendEmptyCart----------------------------
recommendEmptyCart :: String -> String

recommendEmptyCart user = if itemsFlat == [] then "" 
      else getItem itemsFlat (getIndex itemsFlat) where itemsFlat=allItemsFlat (freqListItems user)

allItems []=[] 
allItems ((i,f):xs) = [multipleI i f] ++ (allItems xs)

allItemsFlat list = flatten (allItems list)

multipleI _ 0 = []
multipleI item freq = [item] ++ (multipleI item f) where f=freq-1

-------------------------------recommendBasedOnItemsInCart----------------------------------       
recommendBasedOnItemsInCart :: String -> [String] -> String

recommendBasedOnItemsInCart user cart =if list ==[] then "" 
                                       else getItem list (getIndex list) where list = helpersofia2 (freqListCartAndItems user cart)
helpersofia2 []=[]
helpersofia2 (x:xs)= helpersofia1 x ++ helpersofia2 xs

helpersofia1 (item,0)= []
helpersofia1 (item,n)= [item]++x where x = helpersofia1 (item,n-1)

getIndex list = randomZeroToX (length list -1)

getItem list i = list !! i

--------------------------recommendBasedOnUsers-------------------------
recommendBasedOnUsers :: String -> String
 
recommendBasedOnUsers user = if list ==[] then "" else getItem list (getIndex list) 
								where list = helpersofia2 (freqListUsers user)

---------------------------freqListUsers-----------------------------
freqListUsers:: String -> [(String, Int)]

freqListUsers user = final2 (purchasesIntersection (userIntersection user (getAllUsersStats purchasesHistory)) (allusersbut user (getAllUsersStats purchasesHistory)))

-------------------------purchasesIntersection-------------------------------------
purchasesIntersection:: Eq a => [(a,[(a,Int)])] -> [(a,[(a,[(a,Int)])])] -> [[(a,[(a,Int)])]]

purchasesIntersection _ [] = [] 
purchasesIntersection lista ((user,lista2):xs) =(purchaseshelper lista lista2) : (purchasesIntersection lista xs)


tester f1 f2 = dis( (addfreq f1 f2) ++ (addfreq f2 f1) )

purchaseshelper [] []=[]
purchaseshelper ((item1,f1):x1) ((item2,f2):x2) = if ((length f1) ==0 || (length f2) ==0 )then purchaseshelper x1 x2 else (item1,(tester f1 f2)) : purchaseshelper x1 x2


final2 [lista]= final lista
final2 (x:xs) = tester (final x) (final2 xs)

final [] = []
final [(item,lista)] = lista
final ((item1,list1):list3 ) = tester list1 (final list3)

allusersbut user [] = []
allusersbut user ((user1,listaya):listayas) =  if user /= user1 then (user1,listaya): (allusersbut user listayas) else allusersbut user listayas

--------------------------recommend----------------------------
recommend :: String -> [String] -> String

recommend user cart = if item == "" then randomItem items else recommendhelper2 user cart where item = recommendhelper2 user cart

randomItem items = getItem items (getIndex items) 

recommendhelper2 user cart = if list ==[] then "" 
				else getItem list (getIndex list) where list =recommendhelper user cart 


recommendhelper user  cart = if cart ==[] then [recommendBasedOnUsers user] ++ [recommendEmptyCart user]
                             else [recommendBasedOnItemsInCart user cart] ++ [recommendBasedOnUsers user]
