data QT a = C a | Q (QT a) (QT a) (QT a) (QT a)
  deriving (Eq, Show)

-- 1. buildNSimplify
-- buildNSimplify (C 1) (C 1) (C 1) (C 1) -> C 1
buildNSimplify :: (Eq a) => QT a -> QT a -> QT a -> QT a -> QT a
buildNSimplify (C v1) (C v2) (C v3) (C v4)
  | v1 == v2 && v2 == v3 && v3 == v4 = C v1
buildNSimplify q1 q2 q3 q4 = Q q1 q2 q3 q4

-- 2. simplify
-- simplify test1 -> C 1
simplify :: (Eq a) => QT a -> QT a
simplify (C v) = C v
simplify (Q q1 q2 q3 q4) = buildNSimplify (simplify q1) (simplify q2) (simplify q3) (simplify q4)
