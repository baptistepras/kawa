# Java-like interpreter:

This project was made during my third year of university. It consists in a small programming language inspired by Java, that accepts a basic arithmetic, instructions, classes, while and extends.

# How to use it ?

First do `dune build` to compile the code. `dune clean` will remove any file related to the compilation.

Then do `./kawai.exe <name_of_file>` to execute the code on a file where code is written in Kawa. The file extension must be `.kwa`.

You can add the option `-env` to print the environment at the beginning and end of the execution. For example, do `./kawai.exe tests/fulltest.kwa -env`.

You can also directly use it on my website in the `Projects` section: https://baptistepras.github.io/mywebsite/

The execution of kawai.exe will first launch the lexer, then the parser, then the type checker and finally the interpreter, and will return any result printed or any error encountered. 

# What is full fulltest.kwa ? 

`fulltest.kwa` is a 600 lines code written in Kawa to explore all facets of the language. It contains tens of tests, + tens of tests that are not executed because they are the type of errors you should avoid.

Even though other test files exist, this one has all tests made in other files and is the best one to fully understand the language.

Explanations in fulltest.kwa are in French. A global explanation of the language in English can be found here.

# Language Functionalities

## Typed Language

- You can declare global variables, classes, methods, and attributes specific to each class, as well as local variables specific to each method. A local variable in a method hides a global variable with the same name. At declaration, each variable is initialized with a default value (Int = 0, Bool = false), except for classes with an attribute of type the class itself (recursive classes). In such cases, the attribute is not initialized, and the user is notified of the presence of a recursive class.
- Redefining a global variable, a class, a method, or an attribute within the same class is prohibited, as is redefining a local variable within the same method. Additionally, a class cannot have the same name as a global variable, and a method cannot have the same name as an attribute within the same class. Any other reuse of a name is allowed (e.g., an attribute with the same name as a local or global variable, two methods with the same name but in different classes, a local variable with the same name as the method it belongs to, etc.). Examples are provided below for clarity; see fulltest.kwa for more details.

Prohibited Examples:
```
var int x;                          
var bool x; // Re-declaration of x as a global variable
```
```
var int barbapapa;                 
  class barbapapa {} // Name barbapapa conflicts with a global variable
```
```
class barbapapa {
  method void test(int a) {
    var int a; // Re-declaration of a as local variable (argument of the method + local variable)
  }
}
```
```
class barbapapa {
  attribute int x;
  method void x() {} // Name x conflicts with an attribute
}
```

Allowed Examples:
```
var int x;                          
class barbapapa {
  attribute int x;
}
```
```
class barbapapa {
  method void test() {}
}

class barbamama {
  method void test() {}
}
```
``` 
var int x;                           
  class barbapapa {                    
    method void test() {                  
      var int x;
    }
  }
}
```
```
class barbapapa {
  attribute int barbapapa;
}
```

## Basics

- Supports standard arithmetic and includes the following operators: `<, <=, >, >=, !, -, +, *, /, %, ==, !=, ===, =/=, &&, ||`. Note that `==` and `!=` test physical equality/inequality. `===` and `=/=` test structural equality/inequality and `-` works as both a binary and unary operator when preceding an Int.

- Supported instructions include return, if else, if (without else), and print. Printing method can display Int and Bool. For a Void, it outputs a new line. If used on a class, raise an error.


## Classes and methods

- All reachable branches in methods must have a return statement of the correct type (or no return for void). Special attention is given to if else statements to ensure all branches contain a return.

Prohibited Example:
```
method int test() {
  if false { return 42; }
  else {} // Else branch will not have any return of type int
}
```

Allowed Examples:
```
method int test() { // Every branch as a return of type int
  if false { return 42; }
  else { return 43; } //
}
```
```
method int test() { // Else branch is covered by a return of type int after
  if false { return 42; }
  else {} 
  return 43;
}
```

- Object Instantiation: There are three variants of new for creating objects:
```
p = new point; // Creates a point object with default attribute values.
p = new point(2, 1); // Creates a point object and initializes attributes to 2 and 1. Make sure to give the right number of arguments.
p = newc point(2, 1); // Creates a point object, initializes attributes to default values, and calls the constructor method on p. A method named `constructor` must exist. Make sure to give the right number of arguments.
```

- Method and Constructor Validation: Before calling a method or using the new syntax, it verifies the method exists and ensures the number of arguments passed matches the method’s parameter required.

- `this` Keyword: Usable only within methods and refers to the current object. For instance, `this.x` refers to the x attribute of the current object. The attribute must exists.

- Class Extensions: The language manages EXTEND in classes similarly to Java.

- Multiple Variable Declaration: the language allows multiple variables of same type to be declared at once.
```
var int x, y; // Allowed
var int x, x; // Error: Duplicate declaration
```


## Structure of the code

- The structure must contain a main. Global variable declarations and class declaration and definition must happen before the main. Any instruction to execute must be in the main. This is an example of a correct code:
```
var int x;
var bool b;
var paire p;
var triple t;

class paire {
  attribute int x, y;

  method void constructor(int x, int y) {
    this.x = x;
    this.y = y;
  }

  method int test(int n) {
    while n > 0 {
      print(n%2==0);
      n = n - 1;
    }
    return n;
  }
}

class triple extends paire {
  attribute int z;

  method void constructor(int x, int y, int z) {
    this.x = x;
    this.y = y;
    this.z = z;
  }
}

main {
  x = 42;
  b = true;
  p = new paire(1, 2); // new initialize the attributes of p
  t = new triple(1, 2, 3); // newc calls the method constructor on t
  if b {
    print(p.x);
    print(p.y);
    print(t.x);
    print(t.y);
    print(t.z);
    x = p.test(2);
  } else {
    print(x);
  }
}
```


## Others

- Error Messages: Error messages aim to be as clear as possible to help locate the error source. The name of the class or method is included in the error message if applicable. For errors involving variables, attributes, classes, or methods, their names are also specified. Line or column of the error isn't displayed, except for syntax errors.

- Barbaextension: This extension allows the word “barba” to be prefixed to any keyword. Both standard syntax and “barba” syntax are valid and can be used interchangeably. The following are equivalent:
```
var int x;
barbavar barbaint x;
var barbaint x;
```
