import math, macros, unchained

# often given in `[MeV mol⁻¹ cm²]`, but we don't care
let K = 4 * π * N_A * r_e^2 * m_e * c^2

defUnit(g•cm⁻³)
defUnit(cm³•g⁻¹)
defUnit(J•m⁻¹)
defUnit(cm⁻³)
defUnit(g•mol⁻¹)
defUnit(MeV•g⁻¹•cm²)
defUnit(mol⁻¹)
defUnit(keV•cm⁻¹)

proc electronDensity(ρ: g•cm⁻³, Z, A: UnitLess): cm⁻³ =
  # `M_u` defined in `unchained.constants`
  result = N_A * Z * ρ / (A * M_u.to(g•mol⁻¹))

proc I[T](z: float): T =
  ## approximation
  result = (10.eV * z).to(T) # 188.0 eV from NIST table for Ar (Z = 18)

proc betheBloch(ρ: g•cm⁻³, z, Z, A, β: UnitLess): J•m⁻¹ =
  ## result in J / m
  let ec = e^2 / (4 * π * ε_0)
  let lnArg = 2 * m_e * c^2 * β^2 / (I[Joule](Z) * (1 - β^2))
  result = (4 * π / (m_e * c^2) * electronDensity(ρ, Z, A) * z^2 / (β^2) *
    ec^2 * ( ln(lnArg) - β^2 )).to(J•m⁻¹)

proc calcβ(γ: UnitLess): UnitLess =
  result = sqrt(1.0 - 1.0 / (γ^2))

proc betheBlochPDG(z, Z: UnitLess, A: g•mol⁻¹, γ: UnitLess, M: kg): MeV•g⁻¹•cm² =
  ## result in MeV cm² g⁻¹ (normalized by density)
  ## z: charge of particle
  ## Z: charge of particles making up medium
  ## A: atomic mass of particles making up medium
  ## γ: Lorentz factor of particle
  ## M: mass of particle in MeV (or same mass as `m_e` defined as)
  let β = calcβ(γ)
  let W_max = 2 * m_e * c^2 * β^2 * γ^2 / (1 + 2 * γ * m_e / M + (m_e / M)^2)
  let lnArg = 2 * m_e * c^2 * β^2 * γ^2 * W_max / (I[Joule](Z)^2)
  result = (K * z^2 * Z / A * 1.0 / (β^2) * (
    0.5 * ln(lnArg) - β^2
  )).to(MeV•g⁻¹•cm²)

proc density(p: mbar, M: g•mol⁻¹, temp: Kelvin): g•cm⁻³ =
  ## returns the density of the gas for the given pressure.
  ## The pressure is assumed in `mbar` and the temperature (in `K`).
  ## Returns the density in `g / cm^3`
  let gasConstant = 8.314.J•K⁻¹•mol⁻¹ # joule K^-1 mol^-1
  let pressure = p.to(Pa) # pressure in Pa (not necessarily needed to be done manually)
  # convert to `g•cm⁻³` as desired
  result = (pressure * M / (gasConstant * temp)).to(g•cm⁻³)

let Z_Ar = 18.0
let M_Ar = 39.95.g•mol⁻¹ # molar mass. Numerically same as relative atomic mass
let A_Ar = 39.95.UnitLess # relative atomic mass

# Argon density at 20°C at 1050 mbar
let ρAr = density(1050.mbar, M_Ar, temp = 293.15.K)
defUnit(g•L⁻¹)
echo "Density ", ρAr

import seqmath, ggplotnim, sequtils, strformat
proc plotGammas(ρ: g•cm⁻³) =
  ## plots a bunch of different gammas for one set of gas
  let γs = linspace(1.2, 40.0, 1000)
  let βs = γs.mapIt(it.calcβ())
  let ⟨dE_dx⟩_PDG = γs.mapIt((betheBlochPDG(-1, Z_Ar, M_Ar, it, m_μ) * ρ).to(keV•cm⁻¹).float)
  # convert from J/m ⇒ keV/cm
  let ⟨dE_dx⟩_W = βs.mapIt((betheBloch(ρ, -1, Z_Ar, A_Ar, it).to(keV•cm⁻¹)).float)
  let df = seqsToDf(γs, ⟨dE_dx⟩_PDG, ⟨dE_dx⟩_W)
    .gather(["⟨dE_dx⟩_PDG", "⟨dE_dx⟩_W"], key = "Type", value = "⟨dE_dx⟩")
  ggplot(df, aes("γs", "⟨dE_dx⟩", color = "Type")) +
    geom_line() +
    ylab("⟨dE/dx⟩ [keV/cm]") + xlab("γ (Lorentz factor)") +
    ggtitle(&"Mean ionization energy of muons with γ in Ar at {ρ.float:.2e} g/cm³") +
    theme_opaque() +
    ggsave("media/bethe_bloch_gammas.png")

plotGammas(ρAr)
