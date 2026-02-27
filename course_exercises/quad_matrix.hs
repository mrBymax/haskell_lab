data QT a = C a | Q (QT a) (QT a) (QT a) (QT a)
  deriving (Eq, Show)

data Mat a = Mat
  { nexp :: Int,
    mat :: QT a
  }
  deriving (Eq, Show)
