-- Define a new data type `Proxy`
data Proxy : {k : Type} -> k -> Type where
  MkProxy : Proxy a

-- After loading this file in Idris, you can ask for the type of `MkProxy` as follows:

-- > :set showimplicits
-- > :t MkProxy
-- MkProxy : {phTy : Type} -> {a : phTy} -> Proxy {k = phTy} a

-- Note that `MkProxy` thus has 2 implicits arguments: `phTy` (corresponding to
-- `k` above) and `a`. However, though idris does not show this distinction to
-- the user, it treats these variables differently: `a` can be instantiated
-- while `phTy` can not.

-- Type-checks
foo : Proxy List
foo = MkProxy {a = List}

-- does not type-check:
-- "phTy is not an implicit argument of Proxy"
-- bar : Proxy List
-- bar = MkProxy {phTy = Type} {a = List}
