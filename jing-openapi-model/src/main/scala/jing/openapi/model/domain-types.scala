package jing.openapi.model

sealed trait Int64
sealed trait Str
sealed trait Arr[A]
sealed trait Obj[Props]
sealed trait ||[A, B]
sealed trait ::[A, B]
sealed trait DiscriminatedUnion[Variants]
sealed trait Oops[Reason]
