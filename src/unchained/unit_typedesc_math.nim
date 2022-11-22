import units

#[
Implements helpers to perform regular math with pure units, i.e.

`let x = 10 * kg * m / s^2`
to generate a variable of type
`kg•m•s⁻²`
]#

proc `+`*[T: SomeUnit; U: SomeUnit](a: typedesc[T]; b: typedesc[U]): auto =
  result = 1.T + 1.U

proc `+`*[T: SomeUnit; U: SomeUnit](a: T; b: typedesc[U]): auto =
  result = a + 1.U

proc `+`*[T: SomeUnit; U: SomeUnit](a: typedesc[T]; b: U): auto =
  result = 1.T + b



proc `-`*[T: SomeUnit; U: SomeUnit](a: typedesc[T]; b: typedesc[U]): auto =
  result = 1.T - 1.U

proc `-`*[T: SomeUnit; U: SomeUnit](a: T; b: typedesc[U]): auto =
  result = a - 1.U

proc `-`*[T: SomeUnit; U: SomeUnit](a: typedesc[T]; b: U): auto =
  result = 1.T - b



proc `*`*[T: SomeUnit; U: SomeUnit](a: typedesc[T]; b: typedesc[U]): auto =
  result = 1.T * 1.U

proc `*`*[T: SomeUnit; U: SomeUnit](a: T; b: typedesc[U]): auto =
  result = a * 1.U

proc `*`*[T: SomeUnit; U: SomeUnit](a: typedesc[T]; b: U): auto =
  result = 1.T * b



proc `/`*[T: SomeUnit; U: SomeUnit](a: typedesc[T]; b: typedesc[U]): auto =
  result = 1.T / 1.U

proc `/`*[T: SomeUnit; U: SomeUnit](a: T; b: typedesc[U]): auto =
  result = a / 1.U

proc `/`*[T: SomeUnit; U: SomeUnit](a: typedesc[T]; b: U): auto =
  result = 1.T / b



proc `**`*[T: SomeUnit](a: typedesc[T]; b: static int): auto =
  result = 1.T ^ b

proc `^`*[T: SomeUnit](a: typedesc[T]; b: static int): auto =
  result = 1.T ^ b
