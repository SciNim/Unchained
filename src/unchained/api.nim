## This file provides the basic API to handle units and quantities
##
## You can import this, if you intend to implement your own custom unit system.
##
## import unchained / api # the user facing macros & procedures dealing with units
## import unchained / ct_api # the CT logic to define quantities and units
## declareQuantity: ...
## declareUnits: ...

# import the core types, API & utils
import unchained / [utils, core_types, units]

export core_types
export units
export utils

from unchained / define_units import isAUnit
export isAUnit
