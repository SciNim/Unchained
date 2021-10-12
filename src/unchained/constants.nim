import math
import units

## A (so far small) set of (mostly physics related) constants defined
## in the units of Unchained.

let
  c* = 299792458.0.Meter•Second⁻¹
  ε_0* = 8.8541878128e-12.A•s•V⁻¹•m⁻¹
  e* = 1.602176634e-19.C
  m_e* = 9.1093837015e-31.kg
  m_e_c2* = 0.510998928.MeV
  N_A* = 6.02214076e23.mol⁻¹
  M_u* = 0.99999999965e-3.kg•mol⁻¹
  m_μ_eV* = 105.6583755e6.eV # / c²
  m_μ* = 1.883531627e-28.kg # 105.6583755e3 # MeV / c²
  π* = PI
  r_e* = e*e / (4 * π * ε_0 * m_e * c * c) # classical electron radius
  #K = 4 * π * N_A * r_e * r_e * m_e_c2 * (100.0^2)# [MeV mol⁻¹ cm²]
  k* = 1.380649e-23.Joule•Kelvin⁻¹
  hp* = 6.62607015e-34.Joule•Hertz⁻¹
