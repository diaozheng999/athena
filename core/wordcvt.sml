structure WordCvt : WORD_CVT = 
struct

type b = Bit.word
type w8 = AthenaWord8.word
type w32 = AthenaWord32.word
type w64 = AthenaWord64.word
type mv = Word8VectorSlice.slice


open Endian

fun mono vw = 
    Word8VectorSlice.full
	(Word8Vector.tabulate (VectorSlice.length vw,
			       (fn i => (VectorSlice.sub(vw,i)))))

fun poly w = 
    VectorSlice.full
	(Vector.tabulate (Word8VectorSlice.length w,
			  (fn i => (Word8VectorSlice.sub(w,i)))))


local structure T = WordCvtFn (struct structure W1 = Bit
				      structure W2 = AthenaWord8 end)
      open T
in val bto8b = conv BIG
   val bto8l = conv LITTLE end

local structure T = WordCvtFn (struct structure W1 = Bit
				      structure W2 = AthenaWord32 end)
      open T
in val bto32b = conv BIG
   val bto32l = conv LITTLE end

local structure T = WordCvtFn (struct structure W1 = Bit
				      structure W2 = AthenaWord64 end)
      open T
in val bto64b = conv BIG
   val bto64l = conv LITTLE end

local structure T = WordCvtFn (struct structure W1 = AthenaWord8
				      structure W2 = Bit end)
      open T
in val w8tobb = conv BIG
   val w8tobl = conv LITTLE end

local structure T = WordCvtFn (struct structure W1 = AthenaWord8
				      structure W2 = AthenaWord32 end)
      open T
in val w8to32b = conv BIG
   val w8to32l = conv LITTLE end

local structure T = WordCvtFn (struct structure W1 = AthenaWord8
				      structure W2 = AthenaWord64 end)
      open T
in val w8to64b = conv BIG
   val w8to64l = conv LITTLE end

local structure T = WordCvtFn (struct structure W1 = AthenaWord32
				      structure W2 = Bit end)
      open T
in val w32tobb = conv BIG
   val w32tobl = conv LITTLE end

local structure T = WordCvtFn (struct structure W1 = AthenaWord32
				      structure W2 = AthenaWord8 end)
      open T
in val w32to8b = conv BIG
   val w32to8l = conv LITTLE end

local structure T = WordCvtFn (struct structure W1 = AthenaWord32
				      structure W2 = AthenaWord64 end)
      open T
in val w32to64b = conv BIG
   val w32to64l = conv LITTLE end

local structure T = WordCvtFn (struct structure W1 = AthenaWord64
				      structure W2 = Bit end)
      open T
in val w64tobb = conv BIG
   val w64tobl = conv LITTLE end

local structure T = WordCvtFn (struct structure W1 = AthenaWord64
				      structure W2 = AthenaWord8 end)
      open T
in val w64to8b = conv BIG
   val w64to8l = conv LITTLE end

local structure T = WordCvtFn (struct structure W1 = AthenaWord64
				      structure W2 = AthenaWord32 end)
      open T
in val w64to32b = conv BIG
   val w64to32l = conv LITTLE end

end
