# Datatypes

## Standard types
### Show
Classes deriving from `show` means they can be converted to string represtations, mostly useful for IO.
### Read
Classes deriving from `read` means they canb be converted from string representation into datatypes.
### Eq
Classes deriving from `Eq` can be compared with the `==` operator and return a boolean value.
### Ord
Classes deriving from `Ord` can be ordered aka sorted.
### Enum
Classes deriving from `Enum` can be created in order, for example in ranges [1..10] -> [1,2,3,4,5,6,7,8,9,10]
