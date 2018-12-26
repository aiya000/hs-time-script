# :diamond_shape_with_a_dot_inside: Time script :diamond_shape_with_a_dot_inside:

`Time script` = `Vim script` + strong static typing

## Purpose

- Parsing basic (plain) Vim script with Haskell (haskell-vimlparser)
- Writing Vim script codes with strong static types

## About

about.tim

```vim
" Basic types (you can see at `:help type()`)
let x: Int = 42 " Vim script's Number
let s1: String = 'you'
let s2: String = "me"
let y: Float = 1.0
let b: Bool = v:true
let z1: Null = v:null
let z2: Null = v:none
let xs: List String = ['sugar', 'sweet', 'moon']
let ys: Dict Int = {'foo': 10, 'bar': 20}
let F: Int -> String = function('string')

"" [A] returns a value of A
function! F1(x: Int) [String] abort
  return string(a:x)
endfunction

" Time script own types
"" Natural numbers (non negative numbers)
let n: Nat = 10
let m: Nat = 0
""" compile error! (integral signs means Int literals)
"let l: Nat = +1
"let l: Nat = -1

"" Characters
let c: Char = 'x'
""" compile error! (two or more strings cannot assign)
"let d: Char = 'xx'

"" Unions
let foo: Int | Null = v:null
""" compile error! (v:null cannot assign to non Null types)
" let num: Int = v:null

" Tuples
let t: Tuple Char Nat = ['a', 97] " 2 dimensions
let u: Tuple Int String Bool = [-10, 'me', v:true] " 3
""" compile error! (3 dimensional Tuple cannot assign to 2 dimensional Tuple)
"let v: Tuple Null Null = [v:null, v:null, v:null]
""" compile error! (2 dimensional tuple doesn't have the 3rd element)
"echo t[2]

"" Any
let foo: Any = 10
""" Be typed by Any if the variable type omitted
let foo = 'string'

""" Be typed the returned value by Any for the same reason
function! F2() abort
  return 10
endfunction

""" Be typed the argument by Any for the same reason
function! F3(x) abort
  " x is Any
endfunction

" type synonyms
type Map = List (Tuple Char Nat)

let char_code: Map = [
  \ ['a', 97],
  \ ['b', 98],
  \ ['c', 99],
\ ]
```

## Future
### Typings

- Type casts

```vim
" This often raises some problems
let x: Int = 10
let y: Any = x as Any
```

- Generics

```vim
function! Map<A, B>(x: A | Null, F: A -> B) [B | Null] abort
  if a:x is v:null
    return v:null
  endif
  return f(a:x as Any as A)
endfunction
```

- Sum types

```vim
type Mode = <Normal: Null, Insert: Null, Virtual: VKind>
type VKind = <Charwise: Null, Linewise: Null, Blockwise: Null>

let x: Mode = Normal v:null
let y: Mode = Virtual (Charwise v:null)
```

- Structural subtypings

```vim
```

- Type inferences

```vim
" type(y) is Int (Now type(y) is Any)
let x: Int = 10
let y = x
```

- Type references

```vim
let map: type(char_code) = char_code
```

### Extending Vim script

- String interpolations

```vim
let n: Nat = 10

" expressions to be stringified via string()
echo "$n ${n + 1}"
" 10 11

" single quotes doesn't expand (the raw string)
echo '$n ${n + 1}'
" $n ${n + 1}
```

- Allow to name non upper camel for functions

```vim
let to_string = function('string')
```

- Don't require unnecessary back slashes in trivial cases

```vim
let xs = [
  10, 20, 30,
]

echo map(xs, { _, x ->
    f(x) + g(x)
})
```

- Don't require unnecessary quotes in dicts

```vim
echo {foo: 10} == {'foo': 10}
" 1
```
