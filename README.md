# FortranHashDict
A fast and easy-to-use Key-Value Dictionary for Fortran implemented with a hashtable and singly-linked lists.

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
dict_2%init(size=1000, is_mutable=.false.) !! Specify size and is_mutable attributes.
```

Initialization options:
* The expected max number of Key-Value pairs may be specified with the `size=N` argument. 
  - The actual capacity of the dictionary will be prime number not less than the specified size. 
  - **DEFAULT:** `size=101`
* Prevent overwriting existing keys with the `is_mutable=.true.|.false.` argument. 
  - The `is_mutable` attribute may be updated/changed at any time.
  - **DEFAULT:** `is_mutable=.true.`



## Methods and Attributes

TODO:
- [x] List and describe all dictionary "methods".
- [ ] List and describe all dictionary "attributes".



### Methods
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

**KEYS**: (Subroutine) Returns an array of keys in the dictionary.

```fortran
call dict%keys(k=mykeys [, rc=r])  !! mykeys is an allocatable integer array
!! or
call dict%keys(mykeys)
```

**HAS_KEY**: (Logical Function) Checks if a specified key exists in the dictionary. Returns logical true/false.

```fortran
TF = dict%has_key(key=mykey)
!! or 
TF = dict%has_key(mykey)
```

**HASH**: (Integer Function) Computes the hash for a specified integer key. Returns integer.

```fortran
myhash = dict%hash(key=mykey)
!! or
myhash = dict%hash(mykey)
```

**FREE**: Deletes all key-value pairs and deallocates the dictionary.

```fortran
call dict%free()
```


### Attributes

**CAPACITY**:  
**COUNT**:  
**IS_INIT**:  
**IS_MUTABLE**:


-----------------------------------------
### Examples
```fortran
integer :: k, r, v
integer, dimension(N) :: v1      !! Any 1D array of length "N" (incl. allocatable arrays) are OK.
real(8), dimension(N) :: v2      !! Double-precision values only.

!! Specify an integer value: ival=v
k = 1
v = 99
r = 0
dict%put(key=k, ival=v [, rc=r])
```
