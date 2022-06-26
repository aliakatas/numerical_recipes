    module moon_phase_mod

    implicit none

    real, parameter     :: RAD = 3.14159265 / 180.

    contains

    subroutine moon_phase(n, nphase, julian_day, day_fraction)

    integer, intent(in)     :: n, nphase
    integer, intent(out)    :: julian_day
    real, intent(out)       :: day_fraction

    integer                 :: i
    real                    :: am, as, c, t, t2, xtra

    c = n + nphase / 4.
    t = c / 1236.85
    t2 = t ** 2
    as = 359.2242 + 29.105356 * c
    am = 306.0253 + 385.816918 * c + 0.010730 * t2
    julian_day = 2415020 + 28 * n + 7 * nphase
    xtra = 0.75933 + 1.53058868 * c + (1.178e-4 - 1.55e-7 * t) * t2

    if (nphase == 0 .or. nphase == 2) then
        xtra = xtra + (0.1734 - 3.93e-4 * t) * sin(RAD * as) - 0.4068 * sin(RAD * am)
    else if (nphase == 1 .or. nphase == 3) then
        xtra = xtra + (0.1721 - 4.e-4 * t) * sin(RAD * as) - 0.6280 * sin(RAD * am)
    else
        write(*, *) "Unknown phase entered!"
        write(*, *) "Expect invalid results...!"
        julian_day = -1.
        day_fraction = -1.
    end if 

    if (xtra >= 0.) then
        i = int(xtra)
    else 
        i = int(xtra - 1.)
    end if

    julian_day = julian_day + i
    day_fraction = xtra - i
    end subroutine moon_phase

    end module moon_phase_mod
