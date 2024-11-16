{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           AVLTree
import           Test.QuickCheck

main :: IO ()
main = do
    let tree1 = insert 10 Empty
    let tree2 = insert 5 tree1
    let tree3 = insert 15 tree2
    let tree4 = insert 16 tree3
    let tree8 = insert 17 tree4
    let tree9 = insert 8 Empty
    let tree10 = insert 12 tree9
    let tree11 = insert 14 tree10
    let tree12 = insert 19 tree11
    putStrLn "Исходное дерево после вставок:"
    print tree8

    putStrLn "Проверка наличия элементов:"
    putStrLn $ "Содержится 5? " ++ show (contains 5 tree8)
    putStrLn $ "Содержится 10? " ++ show (contains 10 tree8)
    putStrLn $ "Содержится 20? " ++ show (contains 20 tree8)

    let tree5 = delete 5 tree8
    -- let tree6 = delete 15 tree5
    let tree7 = delete 15 tree5
    putStrLn "Дерево после удаления элемента 5 и 15:"
    print tree7

    putStrLn "Полный список элементов дерева:"
    print (toList tree7)

    putStrLn $ "Дерево сбалансировано? " ++ show (isBalanced tree7)

        -- Применение filterTree: выбираем только четные числа
    let filteredTree = filterTree even tree7
    putStrLn $ "AVL-дерево с четными числами: " ++ show (toList filteredTree)

    -- Применение mapTree: инкрементируем каждое значение на 1
    let mappedTree = mapTree (+1) tree7
    putStrLn $ "AVL-дерево после увеличения на 1: " ++ show (toList mappedTree)

    -- Применение foldlAVL: нахождение суммы всех элементов
    let sumLeft = foldlAVL (+) 0 tree7
    putStrLn $ "Сумма всех элементов (с использованием foldl): " ++ show sumLeft

    -- Применение foldrAVL: нахождение суммы всех элементов
    let sumRight = foldrAVL (+) 0 tree7
    putStrLn $ "Сумма всех элементов (с использованием foldr): " ++ show sumRight

    -- Пример с пустым деревом
    let emptyTree = Empty
    putStrLn $ "Сумма элементов в пустом дереве: " ++ show (foldlAVL (+) 0 emptyTree)

    let mergeTree = merge tree8 tree12
    putStrLn $ "Слияние дерева: " ++ show mergeTree

    putStrLn $ "Дерево сбалансировано? " ++ show (isBalanced mergeTree)

isBalanced :: AVLTree a -> Bool
isBalanced Empty = True
isBalanced (Node left _ _ right _) =
    abs (balanceFactor (Node left undefined 0 right undefined)) <= 1 &&
    isBalanced left &&
    isBalanced right

