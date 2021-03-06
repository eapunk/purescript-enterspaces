module Test.Text.Show.Pretty where

import Prelude
import Test.Assert (assert')
import Text.Show.Pretty

main = do
  assert' "FoldToWidth 0" $ prettify defaultOptions {folding = FoldToWidth 0} """
foo:  Abc {fl = 3.14, str' = "\1055\1088\1080\1074\1077\1090, \20154 \\\":}'", ch = '\1078', dat = Yy [1,2,3], tup = (Just (Xx "))"),True), ls = [[10,20,30],[40,50,60],[]]}
bar: Zz (), baz: Yy [1] bar': Zz (), baz': Yy [1]
""" ==
"""
foo:  Abc {
    fl = 3.14,
    str' = "Привет, 人 \\\":}'",
    ch = 'ж',
    dat = Yy [
        1,
        2,
        3 ],
    tup = (
        Just (
            Xx "))" ),
        True ),
    ls = [
        [
            10,
            20,
            30 ],
        [
            40,
            50,
            60 ],
        [] ] }
bar: Zz (), baz: Yy [
    1 ] bar': Zz (), baz': Yy [
    1 ]
"""

  assert' "FoldToWidth 148" $ prettify defaultOptions {folding = FoldToWidth 148} """
foo:  Abc {fl = 3.14, str' = "\1055\1088\1080\1074\1077\1090, \20154 \\\":}'", ch = '\1078', dat = Yy [1,2,3], tup = (Just (Xx "))"),True), ls = [[10,20,30],[40,50,60],[]]}
bar: Zz (), baz: Yy [1] bar': Zz (), baz': Yy [1]
""" ==
"""
foo:  Abc {fl = 3.14, str' = "Привет, 人 \\\":}'", ch = 'ж', dat = Yy [1, 2, 3], tup = (Just (Xx "))"), True), ls = [[10, 20, 30], [40, 50, 60], []]}
bar: Zz (), baz: Yy [1] bar': Zz (), baz': Yy [1]
"""

  assert' "FoldToWidth 145" $ prettify defaultOptions {folding = FoldToWidth 145} """
foo:  Abc {fl = 3.14, str' = "\1055\1088\1080\1074\1077\1090, \20154 \\\":}'", ch = '\1078', dat = Yy [1,2,3], tup = (Just (Xx "))"),True), ls = [[10,20,30],[40,50,60],[]]}
bar: Zz (), baz: Yy [1] bar': Zz (), baz': Yy [1]
""" ==
"""
foo:  Abc {
    fl = 3.14,
    str' = "Привет, 人 \\\":}'",
    ch = 'ж',
    dat = Yy [1, 2, 3],
    tup = (Just (Xx "))"), True),
    ls = [[10, 20, 30], [40, 50, 60], []] }
bar: Zz (), baz: Yy [1] bar': Zz (), baz': Yy [1]
"""

  assert' "FoldToWidth 43" $ prettify defaultOptions {folding = FoldToWidth 43} """
foo:  Abc {fl = 3.14, str' = "\1055\1088\1080\1074\1077\1090, \20154 \\\":}'", ch = '\1078', dat = Yy [1,2,3], tup = (Just (Xx "))"),True), ls = [[10,20,30],[40,50,60],[]]}
bar: Zz (), baz: Yy [1] bar': Zz (), baz': Yy [1]
""" ==
"""
foo:  Abc {
    fl = 3.14,
    str' = "Привет, 人 \\\":}'",
    ch = 'ж',
    dat = Yy [1, 2, 3],
    tup = (Just (Xx "))"), True),
    ls = [[10, 20, 30], [40, 50, 60], []] }
bar: Zz (), baz: Yy [
    1 ] bar': Zz (), baz': Yy [1]
"""

  assert' "FoldToWidth 42" $ prettify defaultOptions {folding = FoldToWidth 42} """
foo:  Abc {fl = 3.14, str' = "\1055\1088\1080\1074\1077\1090, \20154 \\\":}'", ch = '\1078', dat = Yy [1,2,3], tup = (Just (Xx "))"),True), ls = [[10,20,30],[40,50,60],[]]}
bar: Zz (), baz: Yy [1] bar': Zz (), baz': Yy [1]
""" ==
"""
foo:  Abc {
    fl = 3.14,
    str' = "Привет, 人 \\\":}'",
    ch = 'ж',
    dat = Yy [1, 2, 3],
    tup = (Just (Xx "))"), True),
    ls = [
        [10, 20, 30],
        [40, 50, 60],
        [] ] }
bar: Zz (), baz: Yy [
    1 ] bar': Zz (), baz': Yy [1]
"""

  assert' "FoldToWidth 33" $ prettify defaultOptions {folding = FoldToWidth 33} """
foo:  Abc {fl = 3.14, str' = "\1055\1088\1080\1074\1077\1090, \20154 \\\":}'", ch = '\1078', dat = Yy [1,2,3], tup = (Just (Xx "))"),True), ls = [[10,20,30],[40,50,60],[]]}
bar: Zz (), baz: Yy [1] bar': Zz (), baz': Yy [1]
""" ==
"""
foo:  Abc {
    fl = 3.14,
    str' = "Привет, 人 \\\":}'",
    ch = 'ж',
    dat = Yy [1, 2, 3],
    tup = (Just (Xx "))"), True),
    ls = [
        [10, 20, 30],
        [40, 50, 60],
        [] ] }
bar: Zz (), baz: Yy [
    1 ] bar': Zz (), baz': Yy [1]
"""

  assert' "FoldToWidth 32" $ prettify defaultOptions {folding = FoldToWidth 32}
    """foo:  Abc {fl = 3.14, str' = "\1055\1088\1080\1074\1077\1090, \20154 \\\":}'", ch = '\1078', dat = Yy [1,2,3], tup = (Just (Xx "))"),True), ls = [[10,20,30],[40,50,60],[]]}
bar: Zz (), baz: Yy [1] bar': Zz (), baz': Yy [1]""" ==
"""foo:  Abc {
    fl = 3.14,
    str' = "Привет, 人 \\\":}'",
    ch = 'ж',
    dat = Yy [1, 2, 3],
    tup = (
        Just (Xx "))"),
        True ),
    ls = [
        [10, 20, 30],
        [40, 50, 60],
        [] ] }
bar: Zz (), baz: Yy [
    1 ] bar': Zz (), baz': Yy [
    1 ]"""

  assert' "FoldToWidth 100" $ prettify defaultOptions {folding = FoldToWidth 100}
    """baz: Yy [[1]]""" ==
    """baz: Yy [[1]]"""

  assert' "UnfoldLevel 0" $ prettify defaultOptions {folding = UnfoldLevel 0} """
foo:  Abc {fl = 3.14, str' = "\1055\1088\1080\1074\1077\1090, \20154 \\\":}'", ch = '\1078', dat = Yy [1,2,3], tup = (Just (Xx "))"),True), ls = [[10,20,30],[40,50,60],[]]}
bar: Zz (), baz: Yy [1] bar': Zz (), baz': Yy [1]
""" ==
"""
foo:  Abc {fl = 3.14, str' = "Привет, 人 \\\":}'", ch = 'ж', dat = Yy [1, 2, 3], tup = (Just (Xx "))"), True), ls = [[10, 20, 30], [40, 50, 60], []]}
bar: Zz (), baz: Yy [1] bar': Zz (), baz': Yy [1]
"""

  assert' "UnfoldLevel 1" $ prettify defaultOptions {folding = UnfoldLevel 1} """
foo:  Abc {fl = 3.14, str' = "\1055\1088\1080\1074\1077\1090, \20154 \\\":}'", ch = '\1078', dat = Yy [1,2,3], tup = (Just (Xx "))"),True), ls = [[10,20,30],[40,50,60],[]]}
bar: Zz (), baz: Yy [1] bar': Zz (), baz': Yy [1]
""" ==
"""
foo:  Abc {
    fl = 3.14,
    str' = "Привет, 人 \\\":}'",
    ch = 'ж',
    dat = Yy [1, 2, 3],
    tup = (Just (Xx "))"), True),
    ls = [[10, 20, 30], [40, 50, 60], []] }
bar: Zz (), baz: Yy [
    1 ] bar': Zz (), baz': Yy [
    1 ]
"""

  assert' "UnfoldLevel 2" $ prettify defaultOptions {folding = UnfoldLevel 2} """
foo:  Abc {fl = 3.14, str' = "\1055\1088\1080\1074\1077\1090, \20154 \\\":}'", ch = '\1078', dat = Yy [1,2,3], tup = (Just (Xx "))"),True), ls = [[10,20,30],[40,50,60],[]]}
bar: Zz (), baz: Yy [1] bar': Zz (), baz': Yy [1]
""" ==
"""
foo:  Abc {
    fl = 3.14,
    str' = "Привет, 人 \\\":}'",
    ch = 'ж',
    dat = Yy [
        1,
        2,
        3 ],
    tup = (
        Just (Xx "))"),
        True ),
    ls = [
        [10, 20, 30],
        [40, 50, 60],
        [] ] }
bar: Zz (), baz: Yy [
    1 ] bar': Zz (), baz': Yy [
    1 ]
"""

  assert' "UnfoldLevel 3" $ prettify defaultOptions {folding = UnfoldLevel 3} """
foo:  Abc {fl = 3.14, str' = "\1055\1088\1080\1074\1077\1090, \20154 \\\":}'", ch = '\1078', dat = Yy [1,2,3], tup = (Just (Xx "))"),True), ls = [[10,20,30],[40,50,60],[]]}
bar: Zz (), baz: Yy [1] bar': Zz (), baz': Yy [1]
""" ==
"""
foo:  Abc {
    fl = 3.14,
    str' = "Привет, 人 \\\":}'",
    ch = 'ж',
    dat = Yy [
        1,
        2,
        3 ],
    tup = (
        Just (
            Xx "))" ),
        True ),
    ls = [
        [
            10,
            20,
            30 ],
        [
            40,
            50,
            60 ],
        [] ] }
bar: Zz (), baz: Yy [
    1 ] bar': Zz (), baz': Yy [
    1 ]
"""

  assert' "FoldToWidth 37 str2" $ prettify defaultOptions {folding = FoldToWidth 37} """
bar: Zz ([10, 2]), baz: Yy [[1, 2, 3]]
bar: Zz ([1, 2, 3]), baz: Yy [[1, 2]]""" ==
"""
bar: Zz (
    [10, 2] ), baz: Yy [[1, 2, 3]]
bar: Zz ([1, 2, 3]), baz: Yy [[1, 2]]"""

  assert' "FoldToWidth 33 str2" $ prettify defaultOptions {folding = FoldToWidth 33} """
bar: Zz ([10, 2]), baz: Yy [[1, 2, 3]]
bar: Zz ([1, 2, 3]), baz: Yy [[1, 2]]""" ==
"""
bar: Zz (
    [
        10,
        2 ] ), baz: Yy [
    [1, 2, 3] ]
bar: Zz (
    [1, 2, 3] ), baz: Yy [[1, 2]]"""

  assert' "FoldToWidth 32 str2" $ prettify defaultOptions {folding = FoldToWidth 32} """
bar: Zz ([10, 2]), baz: Yy [[1, 2, 3]]
bar: Zz ([1, 2, 3]), baz: Yy [[1, 2]]""" ==
"""
bar: Zz (
    [
        10,
        2 ] ), baz: Yy [
    [1, 2, 3] ]
bar: Zz (
    [
        1,
        2,
        3 ] ), baz: Yy [[1, 2]]"""

  assert' "FoldToWidth 32'" $ let str = """
foo:  Abc {fl = 3.14, str' = "\1055\1088\1080\1074\1077\1090, \20154 \\\":}'", ch = '\1078', dat = Yy [1,2,3], tup = (Just (Xx "))"),True), ls = [[10,20,30],[40,50,60],[]]}
bar: Zz (), baz: Yy [1] bar': Zz (), baz': Yy [1]
""" in prettify defaultOptions {folding = FoldToWidth 32} str == prettify defaultOptions {folding = FoldToWidth 32} (prettify defaultOptions {folding = FoldToWidth 32} str)

  assert' "UnfoldLevel 3'" $ let str = """
foo:  Abc {fl = 3.14, str' = "\1055\1088\1080\1074\1077\1090, \20154 \\\":}'", ch = '\1078', dat = Yy [1,2,3], tup = (Just (Xx "))"),True), ls = [[10,20,30],[40,50,60],[]]}
bar: Zz (), baz: Yy [1] bar': Zz (), baz': Yy [1]
""" in prettify defaultOptions {folding = UnfoldLevel 3} str == prettify defaultOptions {folding = UnfoldLevel 3} (prettify defaultOptions {folding = UnfoldLevel 3} str)

  assert' "Json" $ prettify defaultOptions {folding = UnfoldLevel 2} """
[{"key":"\u04441"}]""" == """
[
    {
        "key":"ф1" } ]"""

  assert' "Whitespace" $ prettify defaultOptions {folding = FoldToWidth 0} """
abc {  -- comment
    1, -- c1
    2  -- c2
}
""" == """
abc {
    -- comment
    1,
    -- c1
    2  -- c2 }
"""

  assert' "Whitespace 2" $ prettify defaultOptions {folding = UnfoldLevel 0} """
{    	
	"es": [    	
		{    	
			"id"  :  1	,    	
			"name": "one"    	
		}    	
		,    	
		{    
            "id": 2  ,    
            "name": "two"    
        }    
    ]    
}    """ == """
{"es": [{"id":  1, "name": "one"}, {"id": 2, "name": "two"}]}    """

  assert' "Remove 1" $ prettify defaultOptions {folding = FoldToWidth 0, remove = ["a1", "a3", "b", "[0]"]} """
bar: Zz (b, x, a10), baz: Yy [a, {a1, a2, a3 = A3}]""" ==
"""
bar: Zz (
    x ), baz: Yy [
    a,
    {
        a2 } ]"""

  assert' "Remove 2" $ prettify defaultOptions {folding = FoldToWidth 0, remove = ["\"name\": "]} """
{"es": [{"id":  1, "name" = "one"}, {"id": 2, "name": "two"}]}""" ==
"""
{
    "es": [
        {
            "id":  1,
            "name"= "one" },
        {
            "id": 2 } ] }"""

  assert' "Remove 3" $ prettify defaultOptions {folding = FoldToWidth 0, remove = ["'name': "]} """
{'es': [{'id':  1, 'name' = 'one'}, {'id': 2, 'name': 'two'}]}""" ==
"""
{
    'es': [
        {
            'id':  1,
            'name' = 'one' },
        {
            'id': 2 } ] }"""

{-
> ghci

:{
data Abc = Abc {
    fl :: Float,
    str' :: String,
    ch :: Char,
    dat :: Xyz,
    tup :: (Maybe Xyz, Bool),
    ls :: [[Int]]
    }
    deriving Show
data Xyz = Xx String | Yy [Int] | Zz ()
    deriving Show

example = do
    let abc = Abc {
            fl = 3.14,
            str' = "Привет, 人 \\\":}'",
            ch = 'ж',
            dat = Yy [1, 2, 3],
            tup = (Just (Xx "))"), True),
            ls = [[10, 20, 30], [40, 50, 60], []]
            }
        zz = Zz ()
        yy = Yy [1]
    putStrLn $ "foo:  " ++ show abc
    putStrLn $ "bar: " ++ show zz ++ ", baz: " ++ show yy ++ " bar': " ++ show zz ++ ", baz': " ++ show yy
:}
example

-}
