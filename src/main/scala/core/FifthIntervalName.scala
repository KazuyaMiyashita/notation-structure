package core

object FifthIntervalName {
  import FifthName._

  object PerUnison extends FifthInterval(C.value)
  object AugUnison extends FifthInterval(Cs.value)

  object DimSecond extends FifthInterval(Dbb.value)
  object MinSecond extends FifthInterval(Db.value)
  object MajSecond extends FifthInterval(D.value)
  object AugSecond extends FifthInterval(Ds.value)

  object DimThird extends FifthInterval(Ebb.value)
  object MinThird extends FifthInterval(Eb.value)
  object MajThird extends FifthInterval(E.value)
  object AugThird extends FifthInterval(Es.value)

  object DimFourth extends FifthInterval(Fb.value)
  object PerFourth extends FifthInterval(F.value)
  object AugFourth extends FifthInterval(Fs.value)

  object DimFifth extends FifthInterval(Gb.value)
  object PerFifth extends FifthInterval(G.value)
  object AugFifth extends FifthInterval(Gs.value)

  object DimSixth extends FifthInterval(Abb.value)
  object MinSixth extends FifthInterval(Ab.value)
  object MajSixth extends FifthInterval(A.value)
  object AugSixth extends FifthInterval(As.value)

  object DimSeventh extends FifthInterval(Bbb.value)
  object MinSeventh extends FifthInterval(Bb.value)
  object MajSeventh extends FifthInterval(B.value)
  object AugSeventh extends FifthInterval(Bs.value)

  object DimOctave extends FifthInterval(Cb.value)
}
