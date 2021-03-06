**Now this project is the implementing phase!**

Please see the test and the test status to check this progress.

# :diamond_shape_with_a_dot_inside: Time script :diamond_shape_with_a_dot_inside:

[![Build Status](https://travis-ci.org/aiya000/hs-time-script.svg?branch=master)](https://travis-ci.org/aiya000/hs-time-script)

**T**yped V**IM** **E**ngergisch **SCRIPT** = `Vim script` + `strong static typing`

- [The introduction](https://aiya000.github.io/Maid/about-time-script/)

### Overview

```vim
"*
 * Comment
 *"
function ExecuteFizzBuzz(): Void  " abort by default
  const xs: List<Int> = range(0, 100)
  const fizzbuzz: List<String> = []

  for x in xs
    call add(    " Add comments anywhere.
      fizzbuzz,  " Multi line without the line continuation '\'.
      s:fizzbuzz(x),
    )
  endfor

  echo string(fizzbuzz)
endfunction

function s:fizzbuzz(x: Int): String
  return
    x % 15 is 0 ? 'FizzBuzz' :
    x %  5 is 0 ? 'Fizz' :
    x %  3 is 0 : 'Buzz' : string(x)
endfunction

"*
 * Function variables with naming of snake_case (not PascalCase).
 * snake_case: [a-zA-Z0-9_]+
 * PascalCase: [A-Z][A-Za-z0-9_]+
 *"
const f: () => Void = ::FizzBuzz  " Function references by ::
call f()
```

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
let z2: Null  = v:none  " This is a Null to save `type(v:null) is type(v:none)`
```

```vim
" Compile error!
" Because v:null is not an Int
let num: Int = v:null
```

Also types can be embraced by parens `()`.

This is useful when use [generic types](#generic-types).

```vim
let you: (Int)  = 1000
let me: ((Int)) = 000
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
function F()
  return 10
endfunction

" x is an Any
function G(x)
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

### Void

Void is same as `Nat`.

```vim
function G(): Void
endfunction

" This spec is derived by below Vim script spec
echo type(G()) is v:t_number  " true
```

### Builtin generic types <a name="generic-types"></a>

```vim
let xs: List String = ['sugar', 'sweet', 'moon']
let x:  Dict Int    = {'foo': 10, 'bar': 20}
```

```vim
let xss: List (List Int) = [range(0, 1), range(2, 3)]
let x:   Dict (Dict Int) = {
  'foo': { 'x': 10 },
  'bar': { 'y': 20 },
}
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

### Function types

```vim
let F:  Int -> String     = function('string')
let G:  (Int, Int) -> Int = function('range')

" Same as `Int -> (String -> Bool)`
let H: Int -> String -> Bool = { _ ->
  { _ -> v:true }
}
```

#### Variadic argument

TODO

### Unions

```vim
let foo: Int | Null = v:null
```

### Tuples

```vim
" 2 dimensions
let t1: Tuple Char Nat = ['a', 97]
let t2: Tuple (Tuple Char Nat) String = [t1, 'can be nested']

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

### Objects

```vim
let x: {x: Nat, y: String} = {
  y: '000',
  x: 1000,
}
```

### Others
#### Variadic parameters

```vim
function F(...) abort
  " a:000 is typed by List<any>
endfunction
```

## Advanced features <a name="advanced-features"></a>
### Optional parameter

```vim
function F(x?: Nat)
  echo a:x
endfunction

call F(10)  " 10
call F()    " v:null
```

### Default values for parameters

```vim
function F(x: Nat = 20)
  echo a:x
endfunction

call F(10)  " 10
call F()    " 20
```

### Multi-line comments

```vim
"*
 This
 is
 a
 comment
 *"
```

### Allowing comments anywhere

```vim
command! -bar ContLine  " This is a command
    \ call gift#for#you()
```

### Enable function options by default <a name="function-abort-by-default"></a>

Below options are enabled by default.

- `abort`
- `closure`
- `range`
- `dict` (Only if dict qualified names specified. Like `x.f`, `x.y.f`)

```vim
function s:f()
  throw 'error!'
  echo 'finish.'
endfunction

echo s:f()  " E605: Exception not caught: error!
```

But you can disable `abort` by `[[no-abort]]`.

```vim
function s:g() [[no-abort]]
  throw 'error!'
  echo 'finish.'
endfunction

echo s:g()
" E605: Exception not caught: error!
" finish.
```

Also below same as it.

 - `[[no-closure]]`
 - `[[no-range]]`
 - `[[no-dict]]`

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

##### Named function arguments

```vim
function Log(name, message) abort
    echo a:name .. ': ' .. a:message
endfunction

call Log(
    message: 'howdy!',
    name: 'flowey',
    )
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
### Calling functions without `:call`

```vim
F()

" In this future, other than 'call', 'return', and 'echo' must be called with the prefix `:`.
:vsplit
:split

" Conversely, 'call', 'return', and 'echo' must not be called with the prefix `:`.
call F()
echo 10
function G() abort
  return 10
endfunction

" This is able to be parsed, but without any optimizations of parsing.
:call F()
:echo 10
```

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

### enum and pattern matching

```vim
enum Mode
  Normal
  Command
  Visual(kind: Virtual)  -- Can put one or more fields
endenum

enum Virtual
  Charwise
  Linewise
  Blockwise
endenum
```

```vim
const mode: Mode = Visual(Charwise)

const symbol =
  " All enum values must be exhaustive.
  match(mode)
    " Simple
    Normal -> 'n'
    Command -> 'c'
    " Nested fields
    Visual(Charwise) -> 'v'
    Visual(Linewise) -> 'V'
    Visual(Blockwise) -> ''
  endmatch
```

### Decorators

```vim
@DeepConst
const x = {xx: 10}

@Assert({ x, y -> y > 0 })
function Div(x, y) abort
  return x / y
endfunction

function DeepConst(ast_of_binding: AST): Either CompileError AST
  " Modifying of ast_of_binding
endfunction

function Assert(ast_of_div: AST): Either CompileError AST
  " Return a CompileError value if you want to abort compiling
endfunction
```

### Structural subtyping

```vim
const a: {x: Nat, y: Char} = {
  x: 10,
  y: 'a',
}

function F(x: {x: Nat}): Void
endfunction

call F(a)
```

### ???

TODO

```vim
type IntContainer = {
  " An associated type
  type Element = Int

  " A property
  values: List Int

  " A function (
  map(f: Int -> Int): Void
}
```

### Type inferences

```vim
let x: Int = some
let y = x  " Now y is an Int
```

### Type references

```vim
let map: type(char_code) = char_code
```
