type namedterm =
    NamedVar of string
  | NamedAbs of string * namedterm
  | NamedApp of namedterm * namedterm

type dbterm =
    DBVar of int
  | DBAbs of dbterm
  | DBApp of dbterm * dbterm
