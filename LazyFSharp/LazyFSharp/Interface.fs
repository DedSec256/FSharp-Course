module Interface

type ILazy<'a> =
    abstract member Get: unit -> 'a
