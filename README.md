# FortranHashDict
A fast and easy to use Key-Value Dictionary implemented with a hashtable and singly-linked lists, for Fortran 2003+.

The Key-Value Dictionary is implemented as a derived type with user-accessible "methods" and "attributes".  
Currently, FortranHashDict supports the Key-Value mappings:

Key     | Value 
--------|--------------------
Integer | Integer
Integer | Arbitrary-length 1D array of Integers
Integer | Arbitrary-length 1D array of double-precision Reals


## Compile and Link
Compile and link the module to your source code. For example, using `gfortran`:

```bash
gfortran -c fhdict.f90
gfortran -o my_executable fhdict.o
```


## Basic Usage

### Type Declaration and Initialization
In your source code, import the FHDICT module with a USE statement: 

```fortran
use fhdict
```
Declare variables using the hashtable dictionary type from FHDICT:

```fortran
type(hashtable) :: dict_1, dict_2
```

Initialize the dictionary:

```fortran
dict_1%init()                              !! Using default settings.
dict_2%init(nitems=1000, is_mutable=.false.) !! Specify max # items and is_mutable attributes.
```



## Methods and Attributes

TODO:
- [x] List and describe all dictionary "methods".
- [x] List and describe all dictionary "attributes".



### Methods
* **INIT**: (Subroutine) Initialize the dictionary.

```fortran
call dict%init([nitems=N] [,] [is_mutable=.true.|.false])
!! or
call dict%init()
!! or
call dict%init(N)
```

Arguments             | Desc                                     | Default
----------------------|------------------------------------------|------------------------
nitems (optional)     | Expected Max # of key-value pairs        | `nitems=101`
is_mutable (optional) | Allow/Prevent overwriting existing keys  | `is_mutable=.true.`

Notes:
* The hash computed for a key will use a prime number *P* not less than the specified nuber of items *N* such that *N<=P*.
* The `is_mutable` attribute may be updated/changed at any time.


* **PUT**: (Subroutine) Places a key-value pair into the dictionary. The key (key) and one of the mutually-exclusive values (ival, val, rvals) are required.

```fortran
call dict%put(key=mykey, val=myvals [, rc=r]) 
```

Arguments        | Desc                                     | Example
---------------- |------------------------------------------|------------------------
key (required)   | Must be a single-precision integer "key" | `key=5`
ival (optional)  | A single-precision integer "value"       | `ival=99`
val  (optional)  | A 1D array of single-precision integers  | `val=[1,2,3]`
rvals (optional) | A 1D array of double-precision reals     | `rvals=[1.d0, 2.d0, 3.d0]`
rc (optional)    | Return code (success=0, error=1)         | `rc=r`


* **GET**: (Subroutine) Given a key, get the value from the dictionary and return it in the supplied (ival, val, rvals) variable. 

```fortran
call dict%get(key=mykey, val=myvals [, rc=r]) 
```

Arguments        | Desc                                     | Example
---------------- |------------------------------------------|------------------------
key (required)   | Must be a single-precision integer "key" | `key=5`
ival (optional)  | A single-precision integer               | `ival=v`
val  (optional)  | A 1D allocatable array of single-precision integers  | `val=v1`
rvals (optional) | A 1D allocatable array of double-precision reals     | `rvals=v2`
rc (optional)    | Return code (success=0, error=1)         | `rc=r`

* **DEL**: (Subroutine) Deletes a key-value pair from the dictionary.

```fortran
call dict%del(key=mykey [, rc=r]) 
!! or
call dict%del(mykey)
```

* **KEYS**: (Subroutine) Returns an array of keys in the dictionary.

```fortran
call dict%keys(k=mykeys [, rc=r])  !! mykeys is an allocatable integer array
!! or
call dict%keys(mykeys)
```

* **HAS_KEY**: (Logical Function) Checks if a specified key exists in the dictionary. Returns logical true/false.

```fortran
TF = dict%has_key(key=mykey)
!! or 
TF = dict%has_key(mykey)
```

* **HASH**: (Integer Function) Computes the hash for a specified integer key. Returns integer.

```fortran
myhash = dict%hash(key=mykey)
!! or
myhash = dict%hash(mykey)
```

* **FREE**: Deletes all key-value pairs and deallocates the dictionary.

```fortran
call dict%free()
```


### Attributes

* **CAPACITY**: The max number of items specified when the dictionary was initialized. 

* **COUNT**: Current number of key-value pairs in dictionary.

* **IS_INIT**: True if dictionary has been initialized.

* **IS_MUTABLE**: True if the value for an existing key may be overwritten. This attribute may be changed to lock/unlock the dictionary at any time.


-----------------------------------------
### Examples
TODO
