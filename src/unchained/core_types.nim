## These are our absoluteley fundamental base types
## Everything else is generated on top of these as distinct versions
## of them.

type
  Unit* = distinct float

  Quantity* = distinct Unit
  CompoundQuantity* = distinct Quantity

  UnitLess* = distinct Unit
