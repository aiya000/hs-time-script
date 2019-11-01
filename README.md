# :diamond_shape_with_a_dot_inside: Time script :diamond_shape_with_a_dot_inside:

[![Build Status](https://travis-ci.org/aiya000/hs-time-script.svg?branch=master)](https://travis-ci.org/aiya000/hs-time-script)

`Time script` = `Vim script` + `strong static typing`

- [The introduction (Japanese)](https://aiya000.github.io/Maid/about-time-script/)

## Purpose

To Vim script

- Introduce strong static types: [here](#types)
- Introduce advanced features: [here](#advanced-features)
- The innovative, the consistency, and the easy to writeing is emphasizer than the backword compatibility
    - Because Vim script definiately saves the backword compatibility

## Types <a name="types"></a>
### Basic

Simular to `:help type()`.

```vim
let x: Int =  42
let y: Int = +10  " signs
let z: Int = -20
```

```vim
let x: String = 'you'
let y: String = "me"
```

```vim
let x:  Float = 1.0
let y:  Bool  = v:true
let z1: Null  = v:null
let z2: Null  = v:none
```

```vim
" Compile error!
" Because v:null is not an Int
let num: Int = v:null
```

### Natural numbers

Meaning non negative numbers

```vim
let n: Nat = 10
let m: Nat = 0
let l: Nat = n - m  " 0

" Compile error!
" Because integral signs means Int literals
let i: Nat = +1
let i: Nat = -1
```

### Characters

```vim
let c: Char = 'x'
```

```vim
" Compile error!
" Because two or more strings cannot be assigned
let d: Char = 'xx'
```

### Any

```vim
let foo: Any = 10
let foo = 'string'  " re-assignment
let foo = v:null    " 
```

In the latest spec, if the variable type omitted, be typed by `Any`.

(In the future, to be inferenced concrete types.)

```vim
let bar = 'string'  " bar is an Any

" The returned value is an Any
function! F()
  return 10
endfunction

" x is an Any
function! G(x)
endfunction
```

### Typing to functions

Please also see [`abort` by default](#function-abort-by-default).

```vim
function F(x: Int): String
  return string(a:x)
endfunction
```

Supporting defining generic functions is planned on [the future spec](#generic).

### Builtin generic types

```vim
let xs: List String = ['sugar', 'sweet', 'moon']
let x:  Dict Int    = {'foo': 10, 'bar': 20}
```

#### Lists

```vim
let xs: List Int = range(0, 10)  " Initialize with 10 elements
let j: Nat = 0
let i: Int = -1

echo xs[i]  " 0
let xs[i] = 999
echo xs[i]  " 999

echo xs[j]  " 10
let xs[j] = 42
echo xs[j]  " 42
```

```vim
let xs: List Int = range(0, 10)
let i: String = 'x'

" Both compile error!
" Because lists can be accessed only by Int or Nat in Time script
let xs[i] = 999
echo xs[i]

" NOTE: Vim script allows using String as an index, it is used as a number 0.
```

#### Dicts <a name="type-dicts"></a>

Please also see [unnecessary-quotes-in-dicts](#unnecessary-quotes-in-dicts).

```vim
let x = Dict Int = {}
let i: String = 'i'

let x[i] = 10
```

```vim
let x = Dict Int = {}
```

Please also see [unnecessary-trivial-back-slashes](#unnecessary-trivial-back-slashes).

```vim
let x: Dict Int = {
  10: 100,  " This index same as a String '10'
}

" Compile error!
" Because 10 is not a String
echo x[10]

" You should use '10' instead on getting the value
echo x['10']
```

```vim
" Compile error!
" Because 'foo' is not an Int
let x: Dict Int = {
    foo: 'foo',
}
```

### Objects

Same as `Dict Any`.

Please also see [unnecessary-trivial-back-slashes](#unnecessary-trivial-back-slashes).

```vim
let x: Object = {
  foo: 10,
  bar: 'bar',
}
```

Please also see [type-dicts](#type-dicts) for this compile error.

```vim
let x: Object = {}

" Compile error!
" Because 0 is not a String
echo x[0]
```

### Function types

```vim
let F:  Int -> String     = function('string')
let G:  (Int, Int) -> Int = function('range')
```

### Unions

```vim
let foo: Int | Null = v:null
```

### Tuples

```vim
" 2 dimensions
let t: Tuple Char Nat = ['a', 97]

" 3 dimensions
let u: Tuple Int String Bool = [-10, 'me', v:true]
```

```vim
" Compile error!
" Because 3 Dimensional Tuple cannot assign into 2 Dimensional
let i: Tuple Null Null = [v:null, v:null, v:null]

" Compile error!
" Because 2 dimensional tuple doesn't have the 3rd element
echo i[2]
```

## Advanced features <a name="advanced-features"></a>
### Multi-line comments

```vim
"*
 This
 is
 a
 comment
 *"
```

### Allowing comments on all tails of lines

```vim
command! -bar ContLine  " This is a command
    \ call gift#for#you()
```

### Function `abort` by default <a name="function-abort-by-default"></a>

```vim
function s:f()
  throw 'error!'
  echo 'finish.'
endfunction

echo s:f()  " E605: Exception not caught: error!
```

Or `no-abort` allows continuations.

```vim
function s:g() [[no-abort]]
  throw 'error!'
  echo 'finish.'
endfunction

echo s:g()
" E605: Exception not caught: error!
" finish.
```

### String interpolations

```vim
let n: Nat = 10

" expressions to be stringified via string()
echo $'$n ${n + 1}'
" 10 11
```

### Allowing to names of non upper cases `[a-z_]+` for function references

```vim
let to_string = function('string')
```

### Don't require unnecessary back-slashes on trivial cases <a name="unnecessary-trivial-back-slashes"></a>

```vim
let xs = [
  10, 20, 30,
]

echo map(xs, { _, x ->
  f(x) + g(x)
})
```

##### Don't require unnecessary quotes without `#{}` (on `{}` notation) dicts <a name="unnecessary-quotes-in-dicts"></a>

Also allowing mixin names both quoted and not quoted.

```vim
echo {foo: 10} == {'foo': 10}
" 1

let x = {
  aaa: 'caramel',     " Allowed because all keys are named by [a-z_]+
  " keba-b: 'sweet',  " Not allowed because a name uses a char '-'
  'keba-b': 'sweet',  " Not allowed because a name uses a char '-'
}
```

##### Assigning new value without declrarations

```vim
let  x = 10
let* x = 20  " Assign new value
```

```vim
" Compile error!
" Because y is never declared.
let* y = 30

let  z = {}
let* z.a = 40  " error too!
```

## Future specs
### Type synonyms

```vim
type Map = List (Tuple Char Nat)

let char_code: Map = [
  ['a', 97],
  ['b', 98],
  ['c', 99],
]
```

### Type casts

```vim
" This often raises some problems
let x: Int = 10
let y: Any = x as Any
```

### Generics <a name="generics"></a>

Naming `[a-z][a-zA-Z0-9_]` to types that is meaning generic type parameter

```vim
" In this case, `a` is a generic type parameter
function Identity(x: a) a
  return a:x
endfunction

let x: Nat = Identity[Nat](10)  " A type specifying
let y: Nat = Identity(20)       " Or the specifying can be omited
```

### Sum types

```vim
type Mode = <Normal: Null, Insert: Null, Virtual: VKind>
type VKind = <Charwise: Null, Linewise: Null, Blockwise: Null>

let x: Mode = Normal v:null
let y: Mode = Virtual (Charwise v:null)
```

### Structural subtypings

TODO

### Type inferences

```vim
let x: Int = some
let y = x  " Now y is an Int
```

### Type references

```vim
let map: type(char_code) = char_code
```
