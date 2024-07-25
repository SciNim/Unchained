import math
import si_units, units

## A (so far small) set of (mostly physics related) constants defined
## in the units of Unchained.

const
  ## The speed of light in vacuum.
  c* = 299792458.0.Meter•Second⁻¹
  ## The vacuum permitivity / electric constant.
  ε_0* = 8.8541878128e-12.A•s•V⁻¹•m⁻¹
  ## Charge of an electron.
  e* = 1.602176634e-19.C
  ## Mass of an electron.
  m_e* = 9.1093837015e-31.kg
  m_e_c2* = 0.510998928.MeV
  ## Mass of a muon.
  m_μ* = 1.883531627e-28.kg # 105.6583755e3 # MeV / c²
  m_μ_eV* = 105.6583755e6.eV # / c²
  ## The Avogadro constant. The number of particles in one mole of substance, per definition.
  N_A* = 6.02214076e23.mol⁻¹
  ## The 'molar mass constant'. Prior to redefinition in 2019 it was defined as exactly
  ## M_u = 1 g/mol. It relates the standard atomic weight of an element with its
  ## mas for one mole of substance.
  M_u* = 0.99999999965e-3.kg•mol⁻¹
  ## The 'atomic mass constant', the "base mass" for atomic scale objects, defined as
  ## `m_u = 1/12 · m(¹²C) = 1 Dalton`
  ## i.e. the average mass of a single nucleon in a bound atom.
  m_u* = M_u / N_A
  π* = PI
  ## The classical electron radius.
  r_e* = e*e / (4 * π * ε_0 * m_e * c * c)
  #K = 4 * π * N_A * r_e * r_e * m_e_c2 * (100.0^2)# [MeV mol⁻¹ cm²]
  ## The Boltzmann constant
  k_B* = 1.380649e-23.Joule•Kelvin⁻¹
  ## Planck's constant
  hp* = 6.62607015e-34.Joule•Hertz⁻¹
  ## Reduced Planck's constant
  hp_bar* = hp / (2 * π)
  ## Newton's constant. Note: We do not use the common variabl `G`, as it obviously conflicts
  ## with the `Giga` prefix in the general case. Feel free to locally define it as such.
  G_Newton* = 6.6743e-11.N•m²•kg⁻²
