type 'a slice = 'a VectorSlice.slice
signature WORD_CVT =
sig

    type b
    type w8
    type w32
    type w64
    type mv

    datatype endian = LITTLE | BIG


    val bto8b : b slice -> w8 slice
    val bto8l : b slice -> w8 slice
    val bto32b : b slice -> w32 slice
    val bto32l : b slice -> w32 slice
    val bto64b : b slice -> w64 slice
    val bto64l : b slice -> w64 slice

    val w8tobb : w8 slice -> b slice
    val w8tobl : w8 slice -> b slice
    val w8to32b : w8 slice -> w32 slice
    val w8to32l : w8 slice -> w32 slice
    val w8to64b : w8 slice -> w64 slice
    val w8to64l : w8 slice -> w64 slice

    val w32tobb : w32 slice -> b slice
    val w32tobl : w32 slice -> b slice
    val w32to8l : w32 slice -> w8 slice
    val w32to8b : w32 slice -> w8 slice
    val w32to64l : w32 slice -> w64 slice
    val w32to64b : w32 slice -> w64 slice

    val w64tobb : w64 slice -> b slice
    val w64tobl : w64 slice -> b slice
    val w64to8l : w64 slice -> w8 slice
    val w64to8b : w64 slice -> w8 slice
    val w64to32l : w64 slice -> w32 slice
    val w64to32b : w64 slice -> w32 slice


    (* function that converts monovector slices to polymorphic 
       vector slices *)
    val mono : w8 slice -> mv
    val poly : mv -> w8 slice
end
