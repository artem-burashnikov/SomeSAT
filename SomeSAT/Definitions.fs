module SomeSAT.Definitions

type Literal<'T> =
    | Positive of 'T
    | Negative of 'T
