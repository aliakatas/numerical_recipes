    program numerical_recipes

    use moon_phase_mod

    implicit none

    integer         :: n, nphase, julian_day
    real            :: day_fraction
    real            :: hour_dec
    integer         :: hour

    write(*, '(A)') "Starting Numerical Recipes..."
    write(*, '(A)') ""

    write(*, '(A)') "Enter which moon phase you want (since 1st Jan 1900 GMT)"
    read(*, *) n
    
    write(*, '(A)') "Enter what phase of that moon you want (0 = new moon, 1, 2, 3 = last quarter)"
    read(*, *) nphase

    call moon_phase(n, nphase, julian_day, day_fraction)

    write(*, '(I0, A, I0, A)') n, "th moon is happening ", julian_day, " days after 1st Jan 1900 GMT"
    if (nphase == 0) then
        write(*, '(A)', advance='no') "New moon: "
    else if (nphase == 1) then
        write(*, '(A)', advance='no') "First quarter: "
    else if (nphase == 2) then
        write(*, '(A)', advance='no') "Full moon: "
    else if (nphase == 3) then
        write(*, '(A)', advance='no') "Third quarter: "
    else 
        write(*, *) "Unknown phase"
    end if

    hour_dec = 24. * day_fraction
    hour = int(hour_dec)
    write(*, '(I2,A,I2)') hour, ":", int((hour_dec - hour) * 60)

    write(*, *) "Done\nPress Enter to exit"
    read(*, *)

    end program numerical_recipes