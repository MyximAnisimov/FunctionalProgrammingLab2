{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           AVLTree
import           Data.List       (sort)
import           Test.QuickCheck

prop_invariant_insert :: Int -> AVLTree Int -> Bool
prop_invariant_insert x tree =
    let newTree = insert x tree
    in isBalanced newTree

prop_invariant_delete :: Int -> AVLTree Int -> Bool
prop_invariant_delete x tree =
    let newTree = delete x tree
    in isBalanced newTree

isBalanced :: AVLTree a -> Bool
isBalanced Empty = True
isBalanced (Node left _ _ right _) =
    abs (balanceFactor (Node left undefined 0 right undefined)) <= 1 &&
    isBalanced left &&
    isBalanced right

prop_contains :: Int -> AVLTree Int -> Bool
prop_contains x tree =
    let newTree = insert x tree
    in contains x newTree

prop_delete :: Int -> AVLTree Int -> Bool
prop_delete x tree =
    let newTree = insert x tree
    in not (contains x (delete x newTree))

prop_insert_remove :: Int -> AVLTree Int -> Bool
prop_insert_remove x tree =
    let newTree = insert x tree
        finalTree = delete x newTree
    in not (contains x newTree) && contains x newTree
       && (finalTree == delete x tree)

prop_monoid_associativity :: AVLTree Int -> AVLTree Int -> AVLTree Int -> Bool
prop_monoid_associativity a b c =
    let ab = a <> b
        bc = b <> c
    in (ab <> c) `toListCompare` (a <> bc)

prop_monoid_identity :: AVLTree Int -> Bool
prop_monoid_identity tree =
    (tree <> Empty) == tree && (Empty <> tree) == tree

prop_balanced_after_insert :: Int -> AVLTree Int -> Bool
prop_balanced_after_insert x tree =
    let newTree = insert x tree
    in isBalanced newTree

toListCompare :: AVLTree Int -> AVLTree Int -> Bool
toListCompare tree1 tree2 = sort (toList tree1) == sort (toList tree2)

prop_filter_tree :: AVLTree Int -> Bool
prop_filter_tree tree =
    let predicate x = even x
        filteredTree = filterTree predicate tree
        elements = toList filteredTree
    in all predicate elements && length elements <= length (toList tree)

prop_map_tree :: AVLTree Int -> Bool
prop_map_tree tree =
    let f x = x + 1
        mappedTree = mapTree f tree
        mappedElements = toList mappedTree
        originalElements = toList tree
        expectedResult = map f originalElements
    in mappedElements == expectedResult

prop_foldlAVL :: AVLTree Int -> Bool
prop_foldlAVL tree =
    let sumFunction acc x = acc + x
        resultFoldl = foldlAVL sumFunction 0 tree
        resultListFold = foldl sumFunction 0 (toList tree)
    in resultFoldl == resultListFold

prop_foldrAVL :: AVLTree Int -> Bool
prop_foldrAVL tree =
    let sumFunction x acc = x + acc
        resultFoldr = foldrAVL sumFunction 0 tree
        resultListFold = foldr sumFunction 0 (toList tree)
    in resultFoldr == resultListFold

main :: IO ()
main = do
    quickCheck prop_invariant_insert
    quickCheck prop_invariant_delete
    quickCheck prop_contains
    quickCheck prop_delete
    quickCheck prop_insert_remove
    quickCheck prop_monoid_associativity
    quickCheck prop_monoid_identity
    quickCheck prop_balanced_after_insert
    quickCheck prop_filter_tree
    quickCheck prop_map_tree
    quickCheck prop_foldlAVL
    quickCheck prop_foldrAVL
