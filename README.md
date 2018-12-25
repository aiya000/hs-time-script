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
let s: String = 'you'
let f: Int -> String = function('string') " funcref
let xs: List String = ['sugar', 'sweet', 'moon']
let ys: Dict Int = {'foo': 10, 'bar': 20} " dictionary
let y: Float = 1.0
let b: Bool = v:true " boolean
let z: Null = v:null

"" [A] returns a value of A
function! F(x: Int) [String] abort
  return string(a:x)
endfunction

" Time script own types
"" Natural numbers (non negative numbers)
let m: Nat = 10
let n: Nat = 0
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

"" Any
let foo: Any = 10
""" type Any if you don't specify the type
let foo = 'string'

""" Returns an Any by the foo's same reason
function! G() abort
  return 10
endfunction

" type synonyms
type Map = List (Tuple Char Nat)

let char_code: Map = [
  \ ['a', 97],
  \ ['b', 98],
  \ ['c', 99],
\ ]

" type references
let map: type(char_code) = char_code
```

## Appendix
### Future

- Type inferences

```vim
" type(y) is Int (Now type(y) is Any)
let x: Int = 10
let y = x
```

- Sum types

```vim
type Mode = <Normal: Null, Insert: Null, Virtual: VKind>
type VKind = <Charwise: Null, Linewise: Null, Blockwise: Null>

let x: Mode = Normal v:null
let y: Mode = Virtual (Charwise v:null)
```

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

- Don't need unnecessary quotes in dicts

```vim
echo {foo: 10} == {'foo': 10}
" 1
```
