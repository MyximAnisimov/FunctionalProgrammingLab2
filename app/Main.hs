{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           AVLTree
import           Test.QuickCheck

main :: IO ()
main = do
    let tree1 = insert 10 Empty
    let tree2 = insert 5 tree1
    let tree3 = insert 15 tree2
    let tree4 = insert 15 tree3

    putStrLn "Исходное дерево после вставок:"
    print tree4

    putStrLn "Проверка наличия элементов:"
    putStrLn $ "Содержится 5? " ++ show (contains 5 tree4)
    putStrLn $ "Содержится 10? " ++ show (contains 10 tree4)
    putStrLn $ "Содержится 20? " ++ show (contains 20 tree4)

    let tree5 = delete 5 tree4
    -- let tree6 = delete 15 tree5
    let tree7 = delete 15 tree5
    putStrLn "Дерево после удаления элемента 5 и 15:"
    print tree7

    putStrLn "Полный список элементов дерева:"
    print (toList tree5)

    putStrLn $ "Дерево сбалансировано? " ++ show (isBalanced tree5)

isBalanced :: AVLTree a -> Bool
isBalanced Empty = True
isBalanced (Node left _ _ right _) =
    abs (balanceFactor (Node left undefined 0 right undefined)) <= 1 &&
    isBalanced left &&
    isBalanced right
