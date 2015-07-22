signature ATHENA =
sig
    include ATHENA_CORE_TOPLEVEL

    structure Core : ATHENA_CORE
    structure Data : ATHENA_DATA
    structure Program : ATHENA_PROGRAM
end
