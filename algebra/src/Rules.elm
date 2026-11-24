module Rules exposing (..)

import Term exposing (..)
import Rule exposing (Rule)
import Nat


arithmeticRule : Rule
arithmeticRule =
    { name = "Arithmetic"
    , isSymmetric = False
    , antecedents = []
    , left = Atom (MetaVar MetaA)
    , right = Atom (MetaVar MetaA)
    }


logicalNegationInTermsOfImplicationRule : Rule
logicalNegationInTermsOfImplicationRule =
    { name = "Logical negation in terms of implication"
    , isSymmetric = False
    , antecedents = []
    , left = Unary Not (Atom (MetaVar MetaA))
    , right = Binary Implication (Atom (MetaVar MetaA)) (Atom Bot)
    }


explosionForImplicationRule : Rule
explosionForImplicationRule =
    { name = "Explosion for implication"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Implication (Atom Bot) (Atom (MetaVar MetaA))
    , right = Atom Top
    }


negationOfFalseRule : Rule
negationOfFalseRule =
    { name = "Negation of false"
    , isSymmetric = False
    , antecedents = []
    , left = Unary Not (Atom Bot)
    , right = Atom Top
    }


negationOfTrueRule : Rule
negationOfTrueRule =
    { name = "Negation of true"
    , isSymmetric = False
    , antecedents = []
    , left = Unary Not (Atom Top)
    , right = Atom Bot
    }


cancellationOfSquaresRule : Rule
cancellationOfSquaresRule =
    { name = "Cancellation of squares"
    , isSymmetric = False
    , antecedents = []
    , left = Binary EqualTo (Unary Square (Atom (MetaVar MetaAlgA))) (Unary Square (Atom (MetaVar MetaAlgB)))
    , right =
          Binary
              Disjunction
              (Binary EqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB)))
              (Binary EqualTo (Atom (MetaVar MetaAlgA)) (Unary Neg (Atom (MetaVar MetaAlgB))))
    }


productOfSquaresRule : Rule
productOfSquaresRule =
    { name = "Product of squares"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Multiplication (Unary Square (Atom (MetaVar MetaAlgA))) (Unary Square (Atom (MetaVar MetaAlgB)))
    , right = Unary Square (Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB)))
    }


squareAsProductRule : Rule
squareAsProductRule =
    { name = "Square as product"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgA))
    , right = Unary Square (Atom (MetaVar MetaAlgA))
    }


idempotencyForDisjunctionRule : Rule
idempotencyForDisjunctionRule =
    { name = "Idempotency for disjunction"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Disjunction (Atom (MetaVar MetaA)) (Atom (MetaVar MetaA))
    , right = Atom (MetaVar MetaA)
    }


idempotencyForConjunctionRule : Rule
idempotencyForConjunctionRule =
    { name = "Idempotency for conjunction"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Conjunction (Atom (MetaVar MetaA)) (Atom (MetaVar MetaA))
    , right = Atom (MetaVar MetaA)
    }


rightAnnihilationForImplication : Rule
rightAnnihilationForImplication =
    { name = "Right annihilation for implication"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Implication (Atom (MetaVar MetaA)) (Atom Top)
    , right = Atom Top
    }


leftIdentityForImplication : Rule
leftIdentityForImplication =
    { name = "Left identity for implication"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Implication (Atom Top) (Atom (MetaVar MetaA))
    , right = Atom (MetaVar MetaA)
    }


basicRelationsRule : Rule
basicRelationsRule =
    { name = "Basic relations"
    , isSymmetric = False
    , antecedents = []
    , left = Atom (MetaVar MetaA)
    , right = Atom (MetaVar MetaB)
    }


basicArithmeticRule : Rule
basicArithmeticRule =
    { name = "Basic arithmetic"
    , isSymmetric = False
    , antecedents = []
    , left = Atom (MetaVar MetaAlgA)
    , right = Atom (MetaVar MetaAlgA)
    }


cancellationForSubtractionRule : Rule
cancellationForSubtractionRule =
    { name = "Cancellation for subtraction"
    , isSymmetric = False
    , antecedents = [ Unary IsReal (Atom (MetaVar MetaAlgC)) ]
    , left =
          Binary
              Subtraction
              (Binary Addition (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgC)))
              (Binary Addition (Atom (MetaVar MetaAlgB)) (Atom (MetaVar MetaAlgC)))
    , right = Binary Subtraction (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    }


cancellationForDivisionRule : Rule
cancellationForDivisionRule =
    { name = "Cancellation for division"
    , isSymmetric = False
    , antecedents =
          [ Unary IsReal (Atom (MetaVar MetaAlgC))
          , Binary NotEqualTo (Atom (MetaVar MetaAlgC)) (Atom (Num Nat.zero))
          ]
    , left =
          Binary
              Division
              (Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgC)))
              (Binary Multiplication (Atom (MetaVar MetaAlgB)) (Atom (MetaVar MetaAlgC)))
    , right = Binary Division (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    }


multiplicationOfInversesRule : Rule
multiplicationOfInversesRule =
    { name = "Multiplication of inverses"
    , isSymmetric = False
    , antecedents = []
    , left =
          Binary
              Multiplication
              (Binary Division (Atom (Num Nat.one)) (Atom (MetaVar MetaAlgA)))
              (Binary Division (Atom (Num Nat.one)) (Atom (MetaVar MetaAlgB)))
    , right =
          Binary
              Division
              (Atom (Num Nat.one))
              (Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB)))
    }


multiplicationOfFractionsRule : Rule
multiplicationOfFractionsRule =
    { name = "Multiplication of fractions"
    , isSymmetric = False
    , antecedents = []
    , left =
          Binary
              Multiplication
              (Binary Division (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB)))
              (Binary Division (Atom (MetaVar MetaAlgC)) (Atom (MetaVar MetaAlgD)))
    , right =
          Binary
              Division
              (Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgC)))
              (Binary Multiplication (Atom (MetaVar MetaAlgB)) (Atom (MetaVar MetaAlgD)))
    }


distributionOfNegationOverMultiplicationRule : Rule
distributionOfNegationOverMultiplicationRule =
    { name = "Distribution of negation over multiplication"
    , isSymmetric = False
    , antecedents = []
    , left = Unary Neg (Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB)))
    , right = Binary Multiplication (Unary Neg (Atom (MetaVar MetaAlgA))) (Atom (MetaVar MetaAlgB))
    }


doubleNegationEliminationRule : Rule
doubleNegationEliminationRule =
    { name = "Double negation elimination"
    , isSymmetric = False
    , antecedents = []
    , left = Unary Neg (Unary Neg (Atom (MetaVar MetaAlgA)))
    , right = Atom (MetaVar MetaAlgA)
    }


nonzeroProductRule : Rule
nonzeroProductRule =
    { name = "Nonzero product"
    , isSymmetric = False
    , antecedents = [ Unary IsReal (Atom (MetaVar MetaAlgA)), Unary IsReal (Atom (MetaVar MetaAlgB)) ]
    , left =
          Binary
              NotEqualTo
              (Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB)))
              (Atom (Num Nat.zero))
    , right =
          Binary
              Conjunction
              (Binary NotEqualTo (Atom (MetaVar MetaAlgA)) (Atom (Num Nat.zero)))
              (Binary NotEqualTo (Atom (MetaVar MetaAlgB)) (Atom (Num Nat.zero)))
    }


zeroProductRule : Rule
zeroProductRule =
    { name = "Zero product"
    , isSymmetric = False
    , antecedents = [ Unary IsReal (Atom (MetaVar MetaAlgA)), Unary IsReal (Atom (MetaVar MetaAlgB)) ]
    , left =
          Binary
              EqualTo
              (Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB)))
              (Atom (Num Nat.zero))
    , right =
          Binary
              Disjunction
              (Binary EqualTo (Atom (MetaVar MetaAlgA)) (Atom (Num Nat.zero)))
              (Binary EqualTo (Atom (MetaVar MetaAlgB)) (Atom (Num Nat.zero)))
    }


dichotomyForEqualToRule : Rule
dichotomyForEqualToRule =
    { name = "Dichotomy"
    , isSymmetric = False
    , antecedents = [ Unary IsReal (Atom (MetaVar MetaAlgA)) ]
    , left =
          Binary
              Disjunction
              (Binary EqualTo (Atom (MetaVar MetaAlgA)) (Atom (Num Nat.zero)))
              (Binary NotEqualTo (Atom (MetaVar MetaAlgA)) (Atom (Num Nat.zero)))
    , right = Atom Top
    }


divisionByZeroForNotEqualToRule : Rule
divisionByZeroForNotEqualToRule =
    { name = "Division by zero for not equal to"
    , isSymmetric = False
    , antecedents = []
    , left = Binary NotEqualTo (Atom (MetaVar MetaAlgA)) (Binary Division (Atom (Num Nat.one)) (Atom (Num Nat.zero)))
    , right = Atom Top
    }


divisionByZeroForEqualToRule : Rule
divisionByZeroForEqualToRule =
    { name = "Division by zero for equal to"
    , isSymmetric = False
    , antecedents = []
    , left = Binary EqualTo (Binary Division (Atom (Num Nat.one)) (Atom (Num Nat.zero))) (Atom (MetaVar MetaAlgA))
    , right = Atom Bot
    }


outsideDomainForNotEqualToRule : Rule
outsideDomainForNotEqualToRule =
    { name = "Outside domain for not equal to"
    , isSymmetric = False
    , antecedents = [ Unary IsNotReal (Atom (MetaVar MetaAlgA)) ]
    , left = Binary NotEqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    , right = Atom Top
    }


outsideDomainForEqualToRule : Rule
outsideDomainForEqualToRule =
    { name = "Outside domain"
    , isSymmetric = False
    , antecedents = [ Unary IsNotReal (Atom (MetaVar MetaAlgA)) ]
    , left = Binary EqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    , right = Atom Bot
    }


notANumberForNotEqualToRule : Rule
notANumberForNotEqualToRule =
    { name = "Not a number for not equal to"
    , isSymmetric = False
    , antecedents = [ Unary Not (Unary IsReal (Atom (MetaVar MetaAlgA))) ]
    , left = Binary NotEqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    , right = Atom Top
    }


notANumberForEqualToRule : Rule
notANumberForEqualToRule =
    { name = "Not a number"
    , isSymmetric = False
    , antecedents = [ Unary Not (Unary IsReal (Atom (MetaVar MetaAlgA))) ]
    , left = Binary EqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    , right = Atom Bot
    }


irreflexivityForNotEqualToRule : Rule
irreflexivityForNotEqualToRule =
    { name = "Irreflexivity for not equal to"
    , isSymmetric = False
    , antecedents = [ Unary IsReal (Atom (MetaVar MetaAlgA)) ]
    , left = Binary NotEqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgA))
    , right = Atom Bot
    }


reflexivityForEqualToRule : Rule
reflexivityForEqualToRule =
    { name = "Reflexivity"
    , isSymmetric = False
    , antecedents = [ Unary IsReal (Atom (MetaVar MetaAlgA)) ]
    , left = Binary EqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgA))
    , right = Atom Top
    }


substitutionForEqualToRule : Rule
substitutionForEqualToRule =
    { name = "Substitution"
    , isSymmetric = True
    , antecedents = [ Binary EqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB)) ]
    , left = Atom (MetaVar MetaAlgA)
    , right = Atom (MetaVar MetaAlgB)
    }


divisionForNotEqualToRule : Rule
divisionForNotEqualToRule =
    { name = "Division with cancellation for not equal to"
    , isSymmetric = False
    , antecedents = [ Binary NotEqualTo (Atom (MetaVar MetaAlgC)) (Atom (Num Nat.zero)) ]
    , left =
          Binary
              NotEqualTo
              (Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgC)))
              (Atom (MetaVar MetaAlgB))
    , right =
          Binary
              NotEqualTo
              (Atom (MetaVar MetaAlgA))
              (Binary
                   Multiplication
                   (Atom (MetaVar MetaAlgB))
                   (Binary Division (Atom (Num Nat.one)) (Atom (MetaVar MetaAlgC)))
              )
    }


divisionForEqualToRule : Rule
divisionForEqualToRule =
    { name = "Transfer by inversion"
    , isSymmetric = False
    , antecedents = [ Binary NotEqualTo (Atom (MetaVar MetaAlgC)) (Atom (Num Nat.zero)) ]
    , left =
          Binary
              EqualTo
              (Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgC)))
              (Atom (MetaVar MetaAlgB))
    , right =
          Binary
              EqualTo
              (Atom (MetaVar MetaAlgA))
              (Binary
                   Multiplication
                   (Atom (MetaVar MetaAlgB))
                   (Binary Division (Atom (Num Nat.one)) (Atom (MetaVar MetaAlgC)))
              )
    }


subtractionForNotEqualToRule : Rule
subtractionForNotEqualToRule =
    { name = "Subtraction with cancellation for not equal to"
    , isSymmetric = False
    , antecedents = []
    , left =
          Binary
              NotEqualTo
              (Binary Addition (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgC)))
              (Atom (MetaVar MetaAlgB))
    , right =
          Binary
              NotEqualTo
              (Atom (MetaVar MetaAlgA))
              (Binary Addition (Atom (MetaVar MetaAlgB)) (Unary Neg (Atom (MetaVar MetaAlgC))))
    }


subtractionForEqualToRule : Rule
subtractionForEqualToRule =
    { name = "Transfer by negation"
    , isSymmetric = False
    , antecedents = []
    , left =
          Binary
              EqualTo
              (Binary Addition (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgC)))
              (Atom (MetaVar MetaAlgB))
    , right =
          Binary
              EqualTo
              (Atom (MetaVar MetaAlgA))
              (Binary Addition (Atom (MetaVar MetaAlgB)) (Unary Neg (Atom (MetaVar MetaAlgC))))
    }


brokenMultiplicationForEqualToRule : Rule
brokenMultiplicationForEqualToRule =
    { name = "Cancellation of multiplication (broken)"
    , isSymmetric = False
    , antecedents = [ Unary IsReal (Atom (MetaVar MetaAlgC)) ]
    , left =
          Binary
              EqualTo
              (Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgC)))
              (Binary Multiplication (Atom (MetaVar MetaAlgB)) (Atom (MetaVar MetaAlgC)))
    , right = Binary EqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    }


multiplicationForNotEqualToRule : Rule
multiplicationForNotEqualToRule =
    { name = "Cancellation of multiplication for not equal to"
    , isSymmetric = False
    , antecedents =
          [ Unary IsReal (Atom (MetaVar MetaAlgC))
          , Binary NotEqualTo (Atom (MetaVar MetaAlgC)) (Atom (Num Nat.zero))
          ]
    , left =
          Binary
              NotEqualTo
              (Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgC)))
              (Binary Multiplication (Atom (MetaVar MetaAlgB)) (Atom (MetaVar MetaAlgC)))
    , right = Binary NotEqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    }


multiplicationForEqualToRule : Rule
multiplicationForEqualToRule =
    { name = "Cancellation of multiplication"
    , isSymmetric = False
    , antecedents =
          [ Unary IsReal (Atom (MetaVar MetaAlgC))
          , Binary NotEqualTo (Atom (MetaVar MetaAlgC)) (Atom (Num Nat.zero))
          ]
    , left =
          Binary
              EqualTo
              (Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgC)))
              (Binary Multiplication (Atom (MetaVar MetaAlgB)) (Atom (MetaVar MetaAlgC)))
    , right = Binary EqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    }


additionForNotEqualToRule : Rule
additionForNotEqualToRule =
    { name = "Cancellation of addition for not equal to"
    , isSymmetric = False
    , antecedents = [ Unary IsReal (Atom (MetaVar MetaAlgC)) ]
    , left =
          Binary
              NotEqualTo
              (Binary Addition (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgC)))
              (Binary Addition (Atom (MetaVar MetaAlgB)) (Atom (MetaVar MetaAlgC)))
    , right = Binary NotEqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    }


additionForEqualToRule : Rule
additionForEqualToRule =
    { name = "Cancellation of addition"
    , isSymmetric = False
    , antecedents = [ Unary IsReal (Atom (MetaVar MetaAlgC)) ]
    , left =
          Binary
              EqualTo
              (Binary Addition (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgC)))
              (Binary Addition (Atom (MetaVar MetaAlgB)) (Atom (MetaVar MetaAlgC)))
    , right = Binary EqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    }


symmetryForNotEqualToRule : Rule
symmetryForNotEqualToRule =
    { name = "Symmetry for not equal to"
    , isSymmetric = True
    , antecedents = []
    , left = Binary NotEqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    , right = Binary NotEqualTo (Atom (MetaVar MetaAlgB)) (Atom (MetaVar MetaAlgA))
    }


symmetryForEqualToRule : Rule
symmetryForEqualToRule =
    { name = "Symmetry"
    , isSymmetric = True
    , antecedents = []
    , left = Binary EqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    , right = Binary EqualTo (Atom (MetaVar MetaAlgB)) (Atom (MetaVar MetaAlgA))
    }


multiplicationOfInverseRule : Rule
multiplicationOfInverseRule =
    { name = "Fraction in terms of inverse"
    , isSymmetric = False
    , antecedents = []
    , left =
          Binary
              Multiplication
              (Atom (MetaVar MetaAlgA))
              (Binary Division (Atom (Num Nat.one)) (Atom (MetaVar MetaAlgB)))
    , right = Binary Division (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    }


negativeIdentityForMultiplicationRule : Rule
negativeIdentityForMultiplicationRule =
    { name = "Negation in terms of negative one"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Multiplication (Unary Neg (Atom (Num Nat.one))) (Atom (MetaVar MetaAlgA))
    , right = Unary Neg (Atom (MetaVar MetaAlgA))
    }


additionOfNegationRule : Rule
additionOfNegationRule =
    { name = "Subtraction in terms of negation"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Addition (Atom (MetaVar MetaAlgA)) (Unary Neg (Atom (MetaVar MetaAlgB)))
    , right = Binary Subtraction (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    }


resultingIdentityForDivisionRule : Rule
resultingIdentityForDivisionRule =
    { name = "Resulting identity for division"
    , isSymmetric = False
    , antecedents =
          [ Unary IsReal (Atom (MetaVar MetaAlgA))
          , Binary NotEqualTo (Atom (MetaVar MetaAlgA)) (Atom (Num Nat.zero))
          ]
    , left = Binary Division (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgA))
    , right = Atom (Num Nat.one)
    }


resultingIdentityForSubtractionRule : Rule
resultingIdentityForSubtractionRule =
    { name = "Resulting identity for subtraction"
    , isSymmetric = False
    , antecedents = [ Unary IsReal (Atom (MetaVar MetaAlgA)) ]
    , left = Binary Subtraction (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgA))
    , right = Atom (Num Nat.zero)
    }


inverseForMultiplicationRule : Rule
inverseForMultiplicationRule =
    { name = "Inverse for multiplication"
    , isSymmetric = False
    , antecedents =
          [ Unary IsReal (Atom (MetaVar MetaAlgA))
          , Binary NotEqualTo (Atom (MetaVar MetaAlgA)) (Atom (Num Nat.zero))
          ]
    , left =
          Binary
              Multiplication
              (Atom (MetaVar MetaAlgA))
              (Binary Division (Atom (Num Nat.one)) (Atom (MetaVar MetaAlgA)))
    , right = Atom (Num Nat.one)
    }


inverseForAdditionRule : Rule
inverseForAdditionRule =
    { name = "Inverse for addition"
    , isSymmetric = False
    , antecedents = [ Unary IsReal (Atom (MetaVar MetaAlgA)) ]
    , left = Binary Addition (Atom (MetaVar MetaAlgA)) (Unary Neg (Atom (MetaVar MetaAlgA)))
    , right = Atom (Num Nat.zero)
    }


annihilationForDivisionRule : Rule
annihilationForDivisionRule =
    { name = "Annihilation for division"
    , isSymmetric = False
    , antecedents =
          [ Unary IsReal (Atom (MetaVar MetaAlgA))
          , Binary NotEqualTo (Atom (MetaVar MetaAlgA)) (Atom (Num Nat.zero))
          ]
    , left = Binary Division (Atom (Num Nat.zero)) (Atom (MetaVar MetaAlgA))
    , right = Atom (Num Nat.zero)
    }


annihilationForMultiplicationRule : Rule
annihilationForMultiplicationRule =
    { name = "Annihilation for multiplication"
    , isSymmetric = False
    , antecedents = [ Unary IsReal (Atom (MetaVar MetaAlgA)) ]
    , left = Binary Multiplication (Atom (Num Nat.zero)) (Atom (MetaVar MetaAlgA))
    , right = Atom (Num Nat.zero)
    }


brokenAnnihilationForMultiplicationRule : Rule
brokenAnnihilationForMultiplicationRule =
    { name = "Annihilation for multiplication (broken)"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Multiplication (Atom (Num Nat.zero)) (Atom (MetaVar MetaAlgA))
    , right = Atom (Num Nat.zero)
    }


rightIdentityForDivisionRule : Rule
rightIdentityForDivisionRule =
    { name = "Right identity for division"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Division (Atom (MetaVar MetaAlgA)) (Atom (Num Nat.one))
    , right = Atom (MetaVar MetaAlgA)
    }


identityForMultiplicationRule : Rule
identityForMultiplicationRule =
    { name = "Identity for multiplication"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (Num Nat.one))
    , right = Atom (MetaVar MetaAlgA)
    }


rightIdentityForSubtractionRule : Rule
rightIdentityForSubtractionRule =
    { name = "Right identity for subtraction"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Subtraction (Atom (MetaVar MetaAlgA)) (Atom (Num Nat.zero))
    , right = Atom (MetaVar MetaAlgA)
    }


identityForAdditionRule : Rule
identityForAdditionRule =
    { name = "Identity for addition"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Addition (Atom (MetaVar MetaAlgA)) (Atom (Num Nat.zero))
    , right = Atom (MetaVar MetaAlgA)
    }


distributionOfMultiplicationOverSubtractionRule : Rule
distributionOfMultiplicationOverSubtractionRule =
    { name = "Distribution of multiplication over subtraction"
    , isSymmetric = False
    , antecedents = []
    , left =
          Binary
              Multiplication
              (Atom (MetaVar MetaAlgA))
              (Binary Subtraction (Atom (MetaVar MetaAlgB)) (Atom (MetaVar MetaAlgC)))
    , right =
          Binary
              Subtraction
              (Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB)))
              (Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgC)))
    }


distributionOfMultiplicationOverAdditionRule : Rule
distributionOfMultiplicationOverAdditionRule =
    { name = "Distribution of multiplication over addition"
    , isSymmetric = False
    , antecedents = []
    , left =
          Binary
              Multiplication
              (Atom (MetaVar MetaAlgA))
              (Binary Addition (Atom (MetaVar MetaAlgB)) (Atom (MetaVar MetaAlgC)))
    , right =
          Binary
              Addition
              (Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB)))
              (Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgC)))
    }


associativityForMultiplicationRule : Rule
associativityForMultiplicationRule =
    { name = "Associativity for multiplication"
    , isSymmetric = False
    , antecedents = []
    , left =
          Binary
              Multiplication
              (Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB)))
              (Atom (MetaVar MetaAlgC))
    , right =
          Binary
              Multiplication
              (Atom (MetaVar MetaAlgA))
              (Binary Multiplication (Atom (MetaVar MetaAlgB)) (Atom (MetaVar MetaAlgC)))
    }


associativityForAdditionRule : Rule
associativityForAdditionRule =
    { name = "Associativity for addition"
    , isSymmetric = False
    , antecedents = []
    , left =
          Binary
              Addition
              (Binary Addition (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB)))
              (Atom (MetaVar MetaAlgC))
    , right =
          Binary
              Addition
              (Atom (MetaVar MetaAlgA))
              (Binary Addition (Atom (MetaVar MetaAlgB)) (Atom (MetaVar MetaAlgC)))
    }


commutativityForMultiplicationRule : Rule
commutativityForMultiplicationRule =
    { name = "Commutativity for multiplication"
    , isSymmetric = True
    , antecedents = []
    , left = Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    , right = Binary Multiplication (Atom (MetaVar MetaAlgB)) (Atom (MetaVar MetaAlgA))
    }


commutativityForAdditionRule : Rule
commutativityForAdditionRule =
    { name = "Commutativity for addition"
    , isSymmetric = True
    , antecedents = []
    , left = Binary Addition (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    , right = Binary Addition (Atom (MetaVar MetaAlgB)) (Atom (MetaVar MetaAlgA))
    }


divisionByZeroForMultiplicationRule : Rule
divisionByZeroForMultiplicationRule =
    { name = "Division by zero for multiplication"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Multiplication (Binary Division (Atom (Num Nat.one)) (Atom (Num Nat.zero))) (Atom (MetaVar MetaAlgA))
    , right = Binary Division (Atom (Num Nat.one)) (Atom (Num Nat.zero))
    }


divisionByZeroForAdditionRule : Rule
divisionByZeroForAdditionRule =
    { name = "Division by zero for addition"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Addition (Binary Division (Atom (Num Nat.one)) (Atom (Num Nat.zero))) (Atom (MetaVar MetaAlgA))
    , right = Binary Division (Atom (Num Nat.one)) (Atom (Num Nat.zero))
    }


inverseForDivisionByZeroRule : Rule
inverseForDivisionByZeroRule =
    { name = "Division by zero for reciprocal"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Division (Atom (Num Nat.one)) (Binary Division (Atom (Num Nat.one)) (Atom (Num Nat.zero)))
    , right = Binary Division (Atom (Num Nat.one)) (Atom (Num Nat.zero))
    }


negationForDivisionByZeroRule : Rule
negationForDivisionByZeroRule =
    { name = "Division by zero for negation"
    , isSymmetric = False
    , antecedents = []
    , left = Unary Neg (Binary Division (Atom (Num Nat.one)) (Atom (Num Nat.zero)))
    , right = Binary Division (Atom (Num Nat.one)) (Atom (Num Nat.zero))
    }


divisionByZeroIsNotANumberRule : Rule
divisionByZeroIsNotANumberRule =
    { name = "Division by zero is not a number"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Division (Atom (MetaVar MetaAlgA)) (Atom (Num Nat.zero))
    , right = Binary Division (Atom (Num Nat.one)) (Atom (Num Nat.zero))
    }


implicationFromEquivalenceRule : Rule
implicationFromEquivalenceRule =
    { name = "Implication in terms of equivalence"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Implication (Atom (MetaVar MetaA)) (Atom (MetaVar MetaB))
    , right = Binary Equivalence (Atom (MetaVar MetaA)) (Binary Conjunction (Atom (MetaVar MetaA)) (Atom (MetaVar MetaB)))
    }


negationFromEquivalenceRule : Rule
negationFromEquivalenceRule =
    { name = "Negation in terms of equivalence"
    , isSymmetric = False
    , antecedents = []
    , left = Unary Not (Atom (MetaVar MetaA))
    , right = Binary Equivalence (Atom (MetaVar MetaA)) (Atom Bot)
    }


reflexivityForEquivalenceRule : Rule
reflexivityForEquivalenceRule =
    { name = "Reflexivity"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Equivalence (Atom (MetaVar MetaA)) (Atom (MetaVar MetaA))
    , right = (Atom Top)
    }


substitutionRule : Rule
substitutionRule =
    { name = "Substitution"
    , isSymmetric = False
    , antecedents = [ Binary Equivalence (Atom (MetaVar MetaA)) (Atom (MetaVar MetaB)) ]
    , left = (Atom (MetaVar MetaA))
    , right = (Atom (MetaVar MetaB))
    }


associativityForDisjunctionRule : Rule
associativityForDisjunctionRule =
    { name = "Associativity for disjunction"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Disjunction (Atom (MetaVar MetaA)) (Binary Disjunction (Atom (MetaVar MetaB)) (Atom (MetaVar MetaC)))
    , right = Binary Disjunction (Binary Disjunction (Atom (MetaVar MetaA)) (Atom (MetaVar MetaB))) (Atom (MetaVar MetaC))
    }


associativityForConjunctionRule : Rule
associativityForConjunctionRule =
    { name = "Associativity for conjunction"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Conjunction (Atom (MetaVar MetaA)) (Binary Conjunction (Atom (MetaVar MetaB)) (Atom (MetaVar MetaC)))
    , right = Binary Conjunction (Binary Conjunction (Atom (MetaVar MetaA)) (Atom (MetaVar MetaB))) (Atom (MetaVar MetaC))
    }


symmetryForEquivalenceRule : Rule
symmetryForEquivalenceRule =
    { name = "Symmetry for equivalence"
    , isSymmetric = True
    , antecedents = []
    , left = Binary Equivalence (Atom (MetaVar MetaA)) (Atom (MetaVar MetaB))
    , right = Binary Equivalence (Atom (MetaVar MetaB)) (Atom (MetaVar MetaA))
    }


commutativityForDisjunctionRule : Rule
commutativityForDisjunctionRule =
    { name = "Commutativity for disjunction"
    , isSymmetric = True
    , antecedents = []
    , left = Binary Disjunction (Atom (MetaVar MetaA)) (Atom (MetaVar MetaB))
    , right = Binary Disjunction (Atom (MetaVar MetaB)) (Atom (MetaVar MetaA))
    }


commutativityForConjunctionRule : Rule
commutativityForConjunctionRule =
    { name = "Commutativity for conjunction"
    , isSymmetric = True
    , antecedents = []
    , left = Binary Conjunction (Atom (MetaVar MetaA)) (Atom (MetaVar MetaB))
    , right = Binary Conjunction (Atom (MetaVar MetaB)) (Atom (MetaVar MetaA))
    }


annihilationForEquivalenceRule : Rule
annihilationForEquivalenceRule =
    { name = "Annihilation for equivalence (bad)"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Equivalence (Atom Bot) (Atom (MetaVar MetaA))
    , right = (Atom Bot)
    }


annihilationForDisjunctionRule : Rule
annihilationForDisjunctionRule =
    { name = "Annihilation for disjunction"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Disjunction (Atom Top) (Atom (MetaVar MetaA))
    , right = (Atom Top)
    }


annihilationForConjunctionRule : Rule
annihilationForConjunctionRule =
    { name = "Annihilation for conjunction"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Conjunction (Atom Bot) (Atom (MetaVar MetaA))
    , right = (Atom Bot)
    }


distributionOfDisjunctionOverConjunction : Rule
distributionOfDisjunctionOverConjunction =
    { name = "Distribution of disjunction over conjunction"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Disjunction (Atom (MetaVar MetaA)) (Binary Conjunction (Atom (MetaVar MetaB)) (Atom (MetaVar MetaC)))
    , right = Binary Conjunction (Binary Disjunction (Atom (MetaVar MetaA)) (Atom (MetaVar MetaB))) (Binary Disjunction (Atom (MetaVar MetaA)) (Atom (MetaVar MetaC)))
    }


distributionOfConjunctionOverDisjunction : Rule
distributionOfConjunctionOverDisjunction =
    { name = "Distribution of conjunction over disjunction"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Conjunction (Atom (MetaVar MetaA)) (Binary Disjunction (Atom (MetaVar MetaB)) (Atom (MetaVar MetaC)))
    , right = Binary Disjunction (Binary Conjunction (Atom (MetaVar MetaA)) (Atom (MetaVar MetaB))) (Binary Conjunction (Atom (MetaVar MetaA)) (Atom (MetaVar MetaC)))
    }


identityForEquivalenceRule : Rule
identityForEquivalenceRule =
    { name = "Identity for equivalence"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Equivalence (Atom (MetaVar MetaA)) (Atom Top)
    , right = (Atom (MetaVar MetaA))
    }


identityForDisjunctionRule : Rule
identityForDisjunctionRule =
    { name = "Identity for disjunction"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Disjunction (Atom (MetaVar MetaA)) (Atom Bot)
    , right = (Atom (MetaVar MetaA))
    }


identityForConjunctionRule : Rule
identityForConjunctionRule =
    { name = "Identity for conjunction"
    , isSymmetric = False
    , antecedents = []
    , left = Binary Conjunction (Atom (MetaVar MetaA)) (Atom Top)
    , right = (Atom (MetaVar MetaA))
    }


explosionRule : Rule
explosionRule =
    { name = "Explosion"
    , isSymmetric = False
    , antecedents = [ (Atom Bot) ]
    , left = (Atom Top)
    , right = (Atom (MetaVar MetaA))
    }


contradictionForNotEqualToRule : Rule
contradictionForNotEqualToRule =
    { name = "Contradiction for not equal to"
    , isSymmetric = False
    , antecedents = [ Binary EqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB)) ]
    , left = Binary NotEqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    , right = Atom Bot
    }


contradictionForEqualToRule : Rule
contradictionForEqualToRule =
    { name = "Contradiction"
    , isSymmetric = False
    , antecedents = [ Binary NotEqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB)) ]
    , left = Binary EqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    , right = Atom Bot
    }


notEqualToInTermsOfLogicalNegationRule : Rule
notEqualToInTermsOfLogicalNegationRule =
    { name = "Not equal to in terms of logical negation"
    , isSymmetric = False
    , antecedents = []
    , left = Unary Not (Binary EqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB)))
    , right = Binary NotEqualTo (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    }


logicalNegationOfFalseRule : Rule
logicalNegationOfFalseRule =
    { name = "Logical negation of false"
    , isSymmetric = False
    , antecedents = []
    , left = Unary Not (Atom Bot)
    , right = Atom Top
    }


logicalNegationOfTrueRule : Rule
logicalNegationOfTrueRule =
    { name = "Logical negation of true"
    , isSymmetric = False
    , antecedents = []
    , left = Unary Not (Atom Top)
    , right = Atom Bot
    }


contradictionRule : Rule
contradictionRule =
    { name = "Contradiction"
    , isSymmetric = False
    , antecedents = [ Unary Not (Atom (MetaVar MetaA)) ]
    , left = Atom (MetaVar MetaA)
    , right = Atom Bot
    }


tautologyRule : Rule
tautologyRule =
    { name = "Tautology"
    , isSymmetric = False
    , antecedents = [ Atom (MetaVar MetaA) ]
    , left = Atom (MetaVar MetaA)
    , right = Atom Top
    }
