module Problems exposing (..)

import Term exposing (..)
import Problem exposing (..)
import Html exposing (Html)
import Html.Attributes
import Render
import Graphics
import Rules exposing (..)
import Nat


allSymbols : List Term
allSymbols =
    [ Unary Neg (Atom (MetaVar MetaAlgA))
    , Binary Addition (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    , Binary Subtraction (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    , Binary Multiplication (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    , Binary Division (Atom (MetaVar MetaAlgA)) (Atom (MetaVar MetaAlgB))
    , Unary Square (Atom (MetaVar MetaAlgA))
    ]


organizedRules =
    [ arithmeticRule
    , identityForAdditionRule
    , identityForMultiplicationRule
    , commutativityForAdditionRule
    , commutativityForMultiplicationRule
    , associativityForAdditionRule
    , associativityForMultiplicationRule
    , distributionOfMultiplicationOverAdditionRule
    , inverseForAdditionRule
    , inverseForMultiplicationRule
    , additionOfNegationRule
    , multiplicationOfInverseRule
    , annihilationForMultiplicationRule
    , negativeIdentityForMultiplicationRule
    , multiplicationOfInversesRule
    , squareAsProductRule
    , productOfSquaresRule
    , substitutionForEqualToRule
    , reflexivityForEqualToRule
    , symmetryForEqualToRule
    , notEqualToInTermsOfLogicalNegationRule
    , zeroProductRule
    , additionForEqualToRule
    , multiplicationForEqualToRule
    , cancellationOfSquaresRule
    , subtractionForEqualToRule
    , divisionForEqualToRule
    , negationOfTrueRule
    , negationOfFalseRule
    , leftIdentityForImplication
    , rightAnnihilationForImplication
    , identityForConjunctionRule
    , identityForDisjunctionRule
    , annihilationForConjunctionRule
    , annihilationForDisjunctionRule
    , idempotencyForConjunctionRule
    , idempotencyForDisjunctionRule
    , commutativityForConjunctionRule
    , commutativityForDisjunctionRule
    , distributionOfConjunctionOverDisjunction
    , distributionOfDisjunctionOverConjunction
    ]


allRules : List { antecedents : List Term , isSymmetric : Bool , left : Term , name : String , right : Term }
allRules =
    [ symmetryForEqualToRule
    , identityForAdditionRule
    , identityForMultiplicationRule
    , commutativityForAdditionRule
    , commutativityForMultiplicationRule
    , associativityForAdditionRule
    , associativityForMultiplicationRule
    , distributionOfMultiplicationOverAdditionRule
    , substitutionForEqualToRule
    , additionForEqualToRule
    , multiplicationForEqualToRule
    , inverseForAdditionRule
    , additionOfNegationRule
    , inverseForMultiplicationRule
    , multiplicationOfInverseRule
    , subtractionForEqualToRule
    , divisionForEqualToRule
    , notEqualToInTermsOfLogicalNegationRule
    , annihilationForMultiplicationRule
    , negativeIdentityForMultiplicationRule
    , multiplicationOfInversesRule
    , cancellationOfSquaresRule
    , zeroProductRule
    , commutativityForDisjunctionRule
    , idempotencyForDisjunctionRule
    , squareAsProductRule
    , productOfSquaresRule
    , reflexivityForEqualToRule
    , rightAnnihilationForImplication
    , leftIdentityForImplication
    , tautologyRule
    , contradictionRule
    , identityForConjunctionRule
    , identityForDisjunctionRule
    , annihilationForConjunctionRule
    , annihilationForDisjunctionRule
    , commutativityForConjunctionRule
    , associativityForConjunctionRule
    , associativityForDisjunctionRule
    , distributionOfConjunctionOverDisjunction
    , distributionOfDisjunctionOverConjunction
    , dichotomyForEqualToRule
    , nonzeroProductRule
    , negationOfTrueRule
    , negationOfFalseRule
    , explosionForImplicationRule
    , logicalNegationInTermsOfImplicationRule
    ]


problems : List StaticProblem
problems =
    [ { description = "Symmetry"
      , notes =
            [ Html.p
                  []
                  [ Html.text
                        "Your first task to make the formula at the top of the screen look exactly like the formula below it. That is, turn "
                  , Render.termSpan (Binary EqualTo (Atom (Num Nat.one)) (Atom AlgX))
                  , Html.text " into "
                  , Render.termSpan (Binary EqualTo (Atom AlgX) (Atom (Num Nat.one)))
                  , Html.text ". You will need to flip the left and right sides using a rule of algebra called "
                  , Html.em [] [ Html.text "symmetry" ]
                  , Html.text ". Here is the rule:"
                  ]
            , Html.p [] [ Render.leftRuleDiv symmetryForEqualToRule ]
            , Html.p
                  []
                  [ Html.text "Later, I will explain exactly how rules work. For now, you just need to apply the rule."
                  ]
            , Html.p
                  []
                  [ Html.text "If you click on the word "
                  , Html.span [ Html.Attributes.class "sans" ] [ Html.text "Notes" ]
                  , Html.text " and then select "
                  , Html.span [ Html.Attributes.class "sans" ] [ Html.text "Transformations" ]
                  , Html.text
                        " from the popup, it will take you to the transformations panel. The only transformation should be symmetry. If you click it, the top formula should change to look exactly like the lower formula."
                  ]
            , Html.p [] [ Html.text "Go ahead and try it." ]
            , Html.p
                  []
                  [ Html.text "A "
                  , Graphics.lowerWhiteTombstone
                  , Html.text
                        " shape should appear at the top-left of the screen to show you’ve solved the problem. Click it to go on to the next one."
                  ]
            , Html.p
                  []
                  [ Html.text
                        "If something goes wrong you can use the undo button on the transformations panel or reset the problem entirely from the menu at the top-right of the screen."
                  ]
            ]
      , start = Binary EqualTo (Atom (Num Nat.one)) (Atom AlgX)
      , finish = Binary EqualTo (Atom AlgX) (Atom (Num Nat.one))
      , rules = [ symmetryForEqualToRule ]
      , scratch = allSymbols
      }
    , { description = "Identity"
      , notes =
            [ Html.p
                  []
                  [ Html.text "Here is a new rule called "
                  , Html.em [] [ Html.text "identity for addition" ]
                  , Html.text "."
                  ]
            , Html.p [] [ Render.leftRuleDiv identityForAdditionRule ]
            , Html.p
                  []
                  [ Html.text "It can be used to eliminate the "
                  , Render.termSpan (Atom (Num Nat.zero))
                  , Html.text
                        " in the top formula. However, it will not initially show up in the transformations panel because the rule does not yet match."
                  ]
            , Html.p
                  []
                  [ Html.text
                        "See the dots between the two formulas? Clicking on them lets you navigate to different parts of the formula. As you navigate, the "
                  , Html.em [] [ Html.text "focus" ]
                  , Html.text
                        " will change and the available transformations in the transformations panel will change with it. The small dots on the left and right focus in to the left and right, while the ring in the center focuses out. Once you have navigated to a position where the rule matches, the rule should work."
                  ]
            , Html.p
                  []
                  [ Html.text
                        "The rule will show up in its reversed form at a number of locations. We'll use that form in the next problem."
                  ]
            ]
      , start = Binary EqualTo (Binary Addition (Atom AlgX) (Atom (Num Nat.zero))) (Atom (Num (Nat.fromInt 3)))
      , finish = Binary EqualTo (Atom AlgX) (Atom (Num (Nat.fromInt 3)))
      , rules = [ identityForAdditionRule, symmetryForEqualToRule ]
      , scratch = allSymbols
      }
    , { description = "Identity in the other direction"
      , notes =
            [ Html.p
                  []
                  [ Html.text "All rules can be used in both directions. Let's add the "
                  , Render.termSpan (Atom (Num Nat.zero))
                  , Html.text " back in by focusing on the "
                  , Render.termSpan (Atom AlgX)
                  , Html.text " and using the reversed form of identity for addition."
                  ]
            ]
      , start = Binary EqualTo (Atom AlgX) (Atom (Num (Nat.fromInt 5)))
      , finish = Binary EqualTo (Binary Addition (Atom AlgX) (Atom (Num Nat.zero))) (Atom (Num (Nat.fromInt 5)))
      , rules = [ identityForAdditionRule, symmetryForEqualToRule ]
      , scratch = allSymbols
      }
    , { description = "Identity for multiplication"
      , notes =
            [ Html.p [] [ Html.text "I've added an identity rule for multiplication as well." ]
            , Html.p [] [ Render.leftRuleDiv identityForMultiplicationRule ]
            , Html.p [] [ Html.text "Now that you've seen a few rules, I'll explain how to read them." ]
            , Html.p
                  []
                  [ Html.text "The "
                  , Render.turnstileHtml
                  , Html.text " symbol is called the "
                  , Html.em [] [ Html.text "turnstile" ]
                  , Html.text ". For now it just sits on the left. In more advanced rules, it will become important."
                  ]
            , Html.p
                  []
                  [ Html.text "The "
                  , Render.longLeftRightArrowHtml
                  , Html.text
                        " symbol means the rule can replace the pattern on the left with the pattern on the right, or the other way around."
                  ]
            , Html.p
                  []
                  [ Html.text "The "
                  , Render.frakturLowerAHtml
                  , Html.text " (a traditional German letter a) is a "
                  , Html.em [] [ Html.text "metavariable" ]
                  , Html.text
                        ". Metavariables are very much like the regular variables you are familiar with except a variable can only refer to a number. A metavariable can refer to any algebraic expression, including expressions containing division by zero."
                  ]
            , Html.p
                  []
                  [ Html.text "Everything else in the rule, such as the "
                  , Render.termSpan (Atom (Num Nat.one))
                  , Html.text ", is matched literally."
                  ]
            , Html.p
                  []
                  [ Html.text
                        "The best way to understand rules is to use them. Don't be afraid to play around; you can always reset the problem using the menu at the top-right."
                  ]
            ]
      , start =
            Binary
                EqualTo
                (Binary Addition (Atom (Num (Nat.fromInt 4))) (Atom (Num Nat.zero)))
                (Binary Multiplication (Atom AlgX) (Atom (Num Nat.one)))
      , finish = Binary EqualTo (Atom AlgX) (Atom (Num (Nat.fromInt 4)))
      , rules = [ identityForAdditionRule, identityForMultiplicationRule, symmetryForEqualToRule ]
      , scratch = allSymbols
      }
    , { description = "Commutativity, associativity, and distribution"
      , notes =
            [ Html.p
                  []
                  [ Html.text
                        "You're probably already familiar with commutativity, associativity, and distribution. I've added five new rules."
                  ]
            , Html.p [] [ Render.leftRuleDiv commutativityForAdditionRule ]
            , Html.p [] [ Render.leftRuleDiv commutativityForMultiplicationRule ]
            , Html.p [] [ Render.leftRuleDiv associativityForAdditionRule ]
            , Html.p [] [ Render.leftRuleDiv associativityForMultiplicationRule ]
            , Html.p [] [ Render.leftRuleDiv distributionOfMultiplicationOverAdditionRule ]
            , Html.p
                  []
                  [ Html.text "For each problem, the complete list of rules which can be used is shown in the rules panel."
                  ]
            ]
      , start =
            Binary
                EqualTo
                (Binary Addition (Atom (Num (Nat.fromInt 2))) (Atom AlgX))
                (Binary Addition (Atom AlgY) (Binary Addition (Atom (Num (Nat.fromInt 5))) (Atom (Num (Nat.fromInt 2)))))
      , finish =
            Binary
                EqualTo
                (Binary Addition (Atom AlgX) (Atom (Num (Nat.fromInt 2))))
                (Binary Addition (Binary Addition (Atom AlgY) (Atom (Num (Nat.fromInt 5)))) (Atom (Num (Nat.fromInt 2))))
      , rules =
            [ identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , symmetryForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Cleaning up distribution"
      , notes =
            [ Html.p [] [ Html.text "When performing distribution, it is often necessary to do some cleanup afterwards." ]
            ]
      , start =
            Binary
                EqualTo
                (Binary Multiplication (Atom AlgY) (Binary Addition (Atom AlgX) (Atom (Num Nat.one))))
                (Atom (Num Nat.zero))
      , finish =
            Binary
                EqualTo
                (Binary Addition (Binary Multiplication (Atom AlgX) (Atom AlgY)) (Atom AlgY))
                (Atom (Num Nat.zero))
      , rules =
            [ identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , symmetryForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Arithmetic"
      , notes =
            [ Html.p
                  []
                  [ Html.text
                        "All the arithmetic you learned in school is still true in algebra. It can be performed with a singe rule called "
                  , Html.em [] [ Html.text "arithmetic" ]
                  , Html.text "."
                  ]
            , Html.p
                  []
                  [ Html.text
                        "This is the only rule which does not have an explicit pattern. Instead, it matches any time it can perform arithmetic."
                  ]
            ]
      , start = Binary EqualTo (Atom AlgX) (Binary Addition (Atom (Num (Nat.fromInt 2))) (Atom (Num (Nat.fromInt 2))))
      , finish = Binary EqualTo (Atom AlgX) (Atom (Num (Nat.fromInt 4)))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , symmetryForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Arithmetic on coefficients"
      , notes =
            [ Html.p
                  []
                  [ Html.text
                        "What makes algebra interesting is how the rules work together to solve problems. This problem does not introduce a new rule. It challenges you to discover a new tactic using the existing rules."
                  ]
            , Html.p
                  []
                  [ Html.text
                        "If you have trouble with this problem, look closely at the list of rules in the rules panel. Examine each rule and think about how it might be used to help solve the problem."
                  ]
            ]
      , start =
            Binary
                EqualTo
                (Atom AlgX)
                (Binary Addition (Binary Multiplication (Atom (Num (Nat.fromInt 2))) (Atom AlgY)) (Atom AlgY))
      , finish = Binary EqualTo (Atom AlgX) (Binary Multiplication (Atom (Num (Nat.fromInt 3))) (Atom AlgY))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , symmetryForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Cancellation of addition"
      , notes =
            [ Html.p
                  []
                  [ Html.text "Let's cancel the "
                  , Render.termSpan (Atom (Num Nat.two))
                  , Html.text " on both sides of the equation. We can do this using the rule "
                  , Html.em [] [ Html.text "cancellation of addition" ]
                  , Html.text "."
                  ]
            , Html.p [] [ Render.leftRuleDiv additionForEqualToRule ]
            , Html.p
                  []
                  [ Html.text "This rule has a "
                  , Html.em [] [ Html.text "condition" ]
                  , Html.text " to the left of the "
                  , Render.turnstileHtml
                  , Html.text " (turnstile). The condition "
                  , Render.termSpan (Unary IsReal (Atom (MetaVar MetaAlgC)))
                  , Html.text
                        " means the rule can only be used if the expression being cancelled stands for a real number. Division by zero is not a number, so expressions containing division by zero cannot be cancelled."
                  ]
            , Html.p
                  []
                  [ Html.text
                        "Variables are always numbers. Complex expressions built using plus and times are always numbers so long as the complex expression does not contain division by zero."
                  ]
            , Html.p
                  []
                  [ Html.text
                        "If you are ever unsure about a rule, just try applying it. If you are permitted to use a rule, it is safe."
                  ]
            ]
      , start =
            Binary
                EqualTo
                (Binary Addition (Atom AlgX) (Atom (Num (Nat.fromInt 2))))
                (Binary Addition (Atom AlgY) (Atom (Num (Nat.fromInt 2))))
      , finish = Binary EqualTo (Atom AlgX) (Atom AlgY)
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Addition on both sides"
      , notes =
            [ Html.p
                  []
                  [ Html.text
                        "The cancellation of addition rule can be used in reverse, adding to both sides. Try using the rule to add "
                  , Render.termSpan (Atom (Num Nat.one))
                  , Html.text
                        " to both sides. With this rule, you need to choose what to add. A dialog should pop up with a list you can choose from."
                  ]
            ]
      , start = Binary EqualTo (Atom AlgX) (Atom AlgY)
      , finish =
            Binary
                EqualTo
                (Binary Addition (Atom AlgX) (Atom (Num Nat.one)))
                (Binary Addition (Atom AlgY) (Atom (Num Nat.one)))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Constructing large numbers"
      , notes =
            [ Html.p
                  []
                  [ Html.text "What if you want to add the number "
                  , Render.termSpan (Atom (Num (Nat.fromInt 6)))
                  , Html.text " to both sides of an equation? Initially, Only the numbers "
                  , Render.termSpan (Atom (Num Nat.zero))
                  , Html.text ", "
                  , Render.termSpan (Atom (Num Nat.one))
                  , Html.text ", and "
                  , Render.termSpan (Atom (Num Nat.two))
                  , Html.text " are listed. Other numbers must be added."
                  ]
            , Html.p
                  []
                  [ Html.text
                        "Everything currently available to choose from is shown in the scratch pad panel. To add an expression, select the scratch pad panel and click the "
                  , Html.span [ Html.Attributes.class "sans" ] [ Html.text "Number" ]
                  , Html.text " button. Then click the number and press "
                  , Html.span [ Html.Attributes.class "sans" ] [ Html.text "OK" ]
                  , Html.text " to finish."
                  ]
            ]
      , start = Binary EqualTo (Atom AlgX) (Atom AlgY)
      , finish =
            Binary
                EqualTo
                (Binary Addition (Atom AlgX) (Atom (Num (Nat.fromInt 6))))
                (Binary Addition (Atom AlgY) (Atom (Num (Nat.fromInt 6))))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Constructing compound expressions"
      , notes =
            [ Html.p
                  []
                  [ Html.text "What if you want to add something like "
                  , Render.termSpan (Binary Multiplication (Atom (Num Nat.two)) (Atom AlgY))
                  , Html.text " to both sides of an equation? "
                  ]
            , Html.p
                  []
                  [ Html.text "Select the scratch pad panel and click the "
                  , Html.span [ Html.Attributes.class "sans" ] [ Html.text "Combine" ]
                  , Html.text " button. Click the operator and then select the left and right sides. Then click "
                  , Html.span [ Html.Attributes.class "sans" ] [ Html.text "OK" ]
                  , Html.text " to finish. You can repeat this process to build arbitrarily large expressions."
                  ]
            , Html.p [] [ Html.text "This problem requires some cleanup after adding to both sides." ]
            ]
      , start = Binary EqualTo (Atom AlgX) (Atom (Num Nat.zero))
      , finish =
            Binary
                EqualTo
                (Binary Addition (Atom AlgX) (Binary Multiplication (Atom (Num Nat.two)) (Atom AlgY)))
                (Binary Multiplication (Atom (Num Nat.two)) (Atom AlgY))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Constructing negative numbers"
      , notes =
            [ Html.p
                  []
                  [ Html.text "To construct the number "
                  , Render.termSpan (Unary Neg (Atom (Num Nat.five)))
                  , Html.text ", first use "
                  , Html.span [ Html.Attributes.class "sans" ] [ Html.text "Number" ]
                  , Html.text " to construct the number "
                  , Render.termSpan (Atom (Num Nat.five))
                  , Html.text " and then use "
                  , Html.span [ Html.Attributes.class "sans" ] [ Html.text "Combine" ]
                  , Html.text " to construct "
                  , Render.termSpan (Unary Neg (Atom (Num Nat.five)))
                  , Html.text "."
                  ]
            ]
      , start = Binary EqualTo (Atom AlgX) (Atom AlgY)
      , finish =
            Binary
                EqualTo
                (Binary Addition (Atom AlgX) (Unary Neg (Atom (Num (Nat.fromInt 5)))))
                (Binary Addition (Atom AlgY) (Unary Neg (Atom (Num (Nat.fromInt 5)))))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Tactics for cancellation"
      , notes = [ Html.p [] [ Html.text "A new tactic must be discovered in order to solve this problem." ] ]
      , start = Binary EqualTo (Binary Addition (Atom AlgX) (Atom (Num (Nat.fromInt 2)))) (Atom (Num (Nat.fromInt 5)))
      , finish = Binary EqualTo (Atom AlgX) (Atom (Num (Nat.fromInt 3)))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Reversing arithmetic"
      , notes =
            [ Html.p
                  []
                  [ Html.text "In algebra it is also possible to reverse arithmetic, replacing "
                  , Render.termSpan (Atom (Num (Nat.fromInt 4)))
                  , Html.text " with "
                  , Render.termSpan (Binary Addition (Atom (Num (Nat.fromInt 2))) (Atom (Num (Nat.fromInt 2))))
                  , Html.text
                        ". However, in the reverse case the expression must appear in the scratch pad. To solve this problem you must add "
                  , Render.termSpan (Binary Addition (Atom (Num (Nat.fromInt 2))) (Atom (Num (Nat.fromInt 2))))
                  , Html.text " to the scratch pad in order to use reverse arithmetic."
                  ]
            ]
      , start = Binary EqualTo (Atom AlgX) (Atom (Num (Nat.fromInt 4)))
      , finish = Binary EqualTo (Atom AlgX) (Binary Addition (Atom (Num (Nat.fromInt 2))) (Atom (Num (Nat.fromInt 2))))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Reversing arithmetic on coefficients"
      , notes =
            [ Html.p
                  []
                  [ Html.text
                        "Reverse arithmetic can be used to perform reverse arithmetic on coefficients, but it takes a few steps."
                  ]
            ]
      , start = Binary EqualTo (Atom AlgX) (Binary Multiplication (Atom (Num Nat.four)) (Atom AlgY))
      , finish =
            Binary
                EqualTo
                (Atom AlgX)
                (Binary
                     Addition
                     (Binary Multiplication (Atom (Num Nat.two)) (Atom AlgY))
                     (Binary Multiplication (Atom (Num Nat.two)) (Atom AlgY))
                )
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Reversing distribution"
      , notes =
            [ Html.p
                  []
                  [ Html.text
                        "This problem requires distribution in reverse, but some work must be done before the distribution rule can be used."
                  ]
            ]
      , start =
            Binary
                EqualTo
                (Atom AlgX)
                (Binary Addition (Binary Multiplication (Atom (Num Nat.three)) (Atom AlgY)) (Atom (Num Nat.six)))
      , finish =
            Binary
                EqualTo
                (Atom AlgX)
                (Binary Multiplication (Atom (Num Nat.three)) (Binary Addition (Atom AlgY) (Atom (Num Nat.two))))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Cancellation of multiplication"
      , notes =
            [ Html.p [] [ Html.text "Here is the rule cancellation of multiplication:" ]
            , Html.p [] [ Render.leftRuleDiv multiplicationForEqualToRule ]
            , Html.p
                  []
                  [ Html.text
                        "There is an additional condition for cancellation of multiplication: the number being cancelled cannot be equal to zero. Later, a bonus problem will explore what happens if the rule does not have this condition."
                  ]
            ]
      , start =
            Binary
                EqualTo
                (Binary Multiplication (Atom (Num (Nat.fromInt 2))) (Atom AlgX))
                (Binary Multiplication (Atom (Num (Nat.fromInt 2))) (Atom AlgY))
      , finish = Binary EqualTo (Atom AlgX) (Atom AlgY)
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Making cancellation available"
      , notes =
            [ Html.p
                  []
                  [ Html.text
                        "This problem requires a great deal of fussing to get everything into position. This takes practice, but eventually becomes second nature."
                  ]
            ]
      , start =
            Binary
                EqualTo
                (Binary Multiplication (Atom (Num (Nat.fromInt 2))) (Atom AlgX))
                (Binary Addition (Binary Multiplication (Atom (Num (Nat.fromInt 6))) (Atom AlgY)) (Atom (Num (Nat.fromInt 2))))
      , finish =
            Binary
                EqualTo
                (Atom AlgX)
                (Binary Addition (Binary Multiplication (Atom (Num (Nat.fromInt 3))) (Atom AlgY)) (Atom (Num Nat.one)))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Conditional equations"
      , notes =
            [ Html.p
                  []
                  [ Html.text "You may not have seen "
                  , Render.implicationHtml
                  , Html.text " in a formula before. It is called "
                  , Html.em [] [ Html.text "implication" ]
                  , Html.text ". Equations can appear to the left and right of the implication arrow."
                  ]
            , Html.p
                  []
                  [ Html.text "This problem asks you to cancel the "
                  , Render.termSpan (Atom AlgY)
                  , Html.text
                        " in the equation on the right-hand side of the implication arrow. Ordinarily it is not possible to use cancelation of multiplication on a variable; a variable could be any number, including "
                  , Render.termSpan (Atom (Num Nat.zero))
                  , Html.text ". This violates the not equal to "
                  , Render.termSpan (Atom (Num Nat.zero))
                  , Html.text " condition of cancellation of multiplication."
                  ]
            , Html.p
                  []
                  [ Html.text
                        "However, whenever you are focused on the right-hand side of an implication arrow, you may assume whatever is on the left-hand side; in this case, "
                  , Render.termSpan (Binary NotEqualTo (Atom AlgY) (Atom (Num Nat.zero)))
                  , Html.text ". This permits cancellation of "
                  , Render.termSpan (Atom AlgY)
                  , Html.text " on the right-hand side."
                  ]
            , Html.p
                  []
                  [ Html.text
                        "The use of the implication arrow is rare in algebra textbooks, but common in computer proof systems such as this. Algebra textbooks often speak of an implicit assumption that denominators in problems are not equal to zero. They may also speak of the domain of a problem being restricted to well-defined cases. In computer proof systems everything must be explicit, so the implicitation arrow is used instead."
                  ]
            ]
      , start =
            Binary
                Implication
                (Binary NotEqualTo (Atom AlgY) (Atom (Num Nat.zero)))
                (Binary
                     EqualTo
                     (Binary Multiplication (Atom AlgX) (Atom AlgY))
                     (Binary Multiplication (Atom (Num (Nat.fromInt 2))) (Atom AlgY))
                )
      , finish =
            Binary
                Implication
                (Binary NotEqualTo (Atom AlgY) (Atom (Num Nat.zero)))
                (Binary EqualTo (Atom AlgX) (Atom (Num (Nat.fromInt 2))))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Bonus: Broken cancellation of multiplication"
      , notes =
            [ Html.p
                  []
                  [ Html.text "Let's examine why the cancellation of multiplication rule has the not equal to "
                  , Render.termSpan (Atom (Num Nat.zero))
                  , Html.text " condition. I've changed the rule to a broken version where this condition has been removed."
                  ]
            , Html.p [] [ Render.leftRuleDiv brokenMultiplicationForEqualToRule ]
            , Html.p
                  []
                  [ Html.text "It turns out, the rule without the condition allows you to break algebra, showing "
                  , Render.termSpan (Binary EqualTo (Atom (Num Nat.zero)) (Atom (Num Nat.one)))
                  , Html.text "."
                  ]
            , Html.p
                  []
                  [ Html.text "The non-broken version of cancellation of multiplication will be used in problems after this one."
                  ]
            ]
      , start = Binary EqualTo (Atom (Num Nat.zero)) (Atom (Num Nat.zero))
      , finish = Binary EqualTo (Atom (Num Nat.zero)) (Atom (Num Nat.one))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            , brokenMultiplicationForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Inverse for addition"
      , notes =
            [ Html.p
                  []
                  [ Html.text "When a number is added to its negation, the result is always "
                  , Render.termSpan (Atom (Num Nat.zero))
                  , Html.text "."
                  ]
            , Html.p [] [ Render.leftRuleDiv inverseForAdditionRule ]
            , Html.p [] [ Html.text "This is known as ", Html.em [] [ Html.text "inverse for addition" ], Html.text "." ]
            ]
      , start = Binary EqualTo (Binary Addition (Atom AlgA) (Unary Neg (Atom AlgA))) (Atom (Num Nat.zero))
      , finish = Binary EqualTo (Atom (Num Nat.zero)) (Atom (Num Nat.zero))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Subtraction"
      , notes =
            [ Html.p [] [ Html.text "This next rule switches between addition of negation and subtraction." ]
            , Html.p [] [ Render.leftRuleDiv additionOfNegationRule ]
            , Html.p
                  []
                  [ Html.text "It is easier to manipulate algebra involving negation. However, subtraction looks nicer."
                  ]
            ]
      , start = Binary EqualTo (Binary Subtraction (Atom AlgX) (Atom (Num Nat.one))) (Atom (Num (Nat.fromInt 2)))
      , finish = Binary EqualTo (Atom AlgX) (Atom (Num (Nat.fromInt 3)))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , additionOfNegationRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Inverse for addition in the other direction"
      , notes =
            [ Html.p
                  []
                  [ Html.text "As with the other rules, it is useful to use inverse for addition in the other direction."
                  ]
            , Html.p [] [ Html.text "This problem requires some cleanup in the end to look nice." ]
            ]
      , start = Binary EqualTo (Atom (Num Nat.zero)) (Atom (Num Nat.zero))
      , finish = Binary EqualTo (Binary Subtraction (Atom AlgA) (Atom AlgA)) (Atom (Num Nat.zero))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , additionOfNegationRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Inverse for multiplication"
      , notes =
            [ Html.p
                  []
                  [ Html.text "Multiplication has a similar inverse rule. Every number except "
                  , Render.termSpan (Atom (Num Nat.zero))
                  , Html.text " multiplied by its inverse is "
                  , Render.termSpan (Atom (Num Nat.one))
                  , Html.text "."
                  ]
            , Html.p [] [ Render.leftRuleDiv inverseForMultiplicationRule ]
            , Html.p [] [ Html.text "In this problem, the implication arrow is used to satisfy the rule's condition." ]
            ]
      , start =
            Binary
                Implication
                (Binary NotEqualTo (Atom AlgA) (Atom (Num Nat.zero)))
                (Binary
                     EqualTo
                     (Binary Multiplication (Atom AlgA) (Binary Division (Atom (Num Nat.one)) (Atom AlgA)))
                     (Atom (Num Nat.one))
                )
      , finish =
            Binary
                Implication
                (Binary NotEqualTo (Atom AlgA) (Atom (Num Nat.zero)))
                (Binary EqualTo (Atom (Num Nat.one)) (Atom (Num Nat.one)))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Fractions"
      , notes =
            [ Html.p [] [ Html.text "Fractions can be used to make formulas look nicer." ]
            , Html.p [] [ Render.leftRuleDiv multiplicationOfInverseRule ]
            ]
      , start =
            Binary
                EqualTo
                (Binary Multiplication (Atom (Num (Nat.fromInt 2))) (Atom AlgX))
                (Binary Addition (Atom AlgY) (Atom (Num Nat.one)))
      , finish =
            Binary
                EqualTo
                (Atom AlgX)
                (Binary Division (Binary Addition (Atom AlgY) (Atom (Num Nat.one))) (Atom (Num (Nat.fromInt 2))))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Transfer by negation"
      , notes =
            [ Html.p
                  []
                  [ Html.text "Here is a rule called "
                  , Html.em [] [ Html.text "transfer by negation" ]
                  , Html.text "."
                  ]
            , Html.p [] [ Render.leftRuleDiv subtractionForEqualToRule ]
            , Html.p
                  []
                  [ Html.text
                        "It moves terms from one side of an equation to the other directly, without having to use the scratch pad."
                  ]
            , Html.p
                  []
                  [ Html.text
                        "In this problem you will derive the rule by showing transfer by negation can be done using the existing rules. I've set the left-hand side as the initial formula and the right-hand side as the goal formula."
                  ]
            , Html.p [] [ Html.text "After this problem transfer by negation will be added to the list of rules." ]
            ]
      , start = Binary EqualTo (Binary Addition (Atom AlgA) (Atom AlgC)) (Atom AlgB)
      , finish = Binary EqualTo (Atom AlgA) (Binary Addition (Atom AlgB) (Unary Neg (Atom AlgC)))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Using transfer by negation"
      , notes = [ Html.p [] [ Html.text "Transfer by negation can be used to quickly solve this problem." ] ]
      , start =
            Binary
                EqualTo
                (Binary Addition (Atom AlgX) (Atom (Num Nat.one)))
                (Binary Addition (Atom AlgY) (Atom (Num Nat.three)))
      , finish = Binary EqualTo (Atom AlgX) (Binary Addition (Atom AlgY) (Atom (Num Nat.two)))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Transfer by inversion"
      , notes =
            [ Html.p [] [ Html.text "Transfer by (multiplicative) inversion is similar to transfer by negation." ]
            , Html.p [] [ Render.leftRuleDiv divisionForEqualToRule ]
            , Html.p
                  []
                  [ Html.text "In this problem you derive the rule in the same way you derived transfer by negation."
                  ]
            , Html.p
                  []
                  [ Html.text "Note that you cannot solve this problem by first multiplying both sides by "
                  , Render.termSpan (Binary Division (Atom (Num Nat.one)) (Atom AlgC))
                  , Html.text ". The reason is, the proof system is not smart enough to figure out "
                  , Render.termSpan (Binary NotEqualTo (Binary Division (Atom (Num Nat.one)) (Atom AlgC)) (Atom (Num Nat.zero)))
                  , Html.text " by itself. However, there is another way to solve the problem."
                  ]
            , Html.p [] [ Html.text "After this problem transfer by inversion will be added to the list of rules." ]
            ]
      , start =
            Binary
                Implication
                (Binary NotEqualTo (Atom AlgC) (Atom (Num Nat.zero)))
                (Binary EqualTo (Binary Multiplication (Atom AlgA) (Atom AlgC)) (Atom AlgB))
      , finish =
            Binary
                Implication
                (Binary NotEqualTo (Atom AlgC) (Atom (Num Nat.zero)))
                (Binary
                     EqualTo
                     (Atom AlgA)
                     (Binary Multiplication (Atom AlgB) (Binary Division (Atom (Num Nat.one)) (Atom AlgC)))
                )
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Using transfer by inversion"
      , notes = [ Html.p [] [ Html.text "Transfer by inversion can be used to quickly solve this problem." ] ]
      , start = Binary EqualTo (Binary Multiplication (Atom (Num Nat.three)) (Atom AlgX)) (Atom (Num Nat.two))
      , finish = Binary EqualTo (Atom AlgX) (Binary Division (Atom (Num Nat.two)) (Atom (Num Nat.three)))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , symmetryForEqualToRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Logical negation"
      , notes =
            [ Html.p
                  []
                  [ Html.text "The relation "
                  , Render.termSpan (Binary NotEqualTo (Atom AlgA) (Atom AlgB))
                  , Html.text " can also be written "
                  , Render.termSpan (Unary Not (Binary EqualTo (Atom AlgA) (Atom AlgB)))
                  , Html.text ". The "
                  , Render.notHtml
                  , Html.text " symbol is called the "
                  , Html.em [] [ Html.text "logical negation" ]
                  , Html.text " symbol and can be read aloud as “not”. I've added a rule to switch between the two forms."
                  ]
            , Html.p [] [ Render.leftRuleDiv notEqualToInTermsOfLogicalNegationRule ]
            ]
      , start = Binary NotEqualTo (Atom AlgA) (Atom AlgB)
      , finish = Unary Not (Binary EqualTo (Atom AlgA) (Atom AlgB))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Manipulating not equal to"
      , notes =
            [ Html.p [] [ Html.text "Let's make use of the previous rule to solve a problem involving not equal to." ]
            ]
      , start =
            Binary
                NotEqualTo
                (Binary Multiplication (Atom (Num (Nat.fromInt 3))) (Atom AlgX))
                (Atom (Num (Nat.fromInt 1)))
      , finish = Binary NotEqualTo (Atom AlgX) (Binary Division (Atom (Num Nat.one)) (Atom (Num (Nat.fromInt 3))))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Modifying a condition"
      , notes =
            [ Html.p
                  []
                  [ Html.text "The left-hand side of an implication arrow may be modified, just like the right-hand side."
                  ]
            , Html.p
                  []
                  [ Html.text
                        "Note that in the goal formula the left-hand side of the implication arrow has been changed, making it possible to solve the equation on the right-hand side."
                  ]
            ]
      , start =
            Binary
                Implication
                (Binary NotEqualTo (Atom AlgA) (Atom (Num Nat.zero)))
                (Binary
                     EqualTo
                     (Binary Division (Atom AlgX) (Binary Multiplication (Atom (Num (Nat.fromInt 2))) (Atom AlgA)))
                     (Atom (Num (Nat.fromInt 3)))
                )
      , finish =
            Binary
                Implication
                (Binary NotEqualTo (Binary Multiplication (Atom (Num (Nat.fromInt 2))) (Atom AlgA)) (Atom (Num Nat.zero)))
                (Binary EqualTo (Atom AlgX) (Binary Multiplication (Atom (Num (Nat.fromInt 6))) (Atom AlgA)))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Arithmetic evaluation of relations"
      , notes =
            [ Html.p
                  []
                  [ Html.text "The arithmetic rule knows "
                  , Render.termSpan (Binary EqualTo (Atom (Num Nat.two)) (Atom (Num Nat.two)))
                  , Html.text " is true. It replaces the equation with "
                  , Render.termSpan (Atom Top)
                  , Html.text ", which is a symbol which means true."
                  ]
            , Html.p
                  []
                  [ Html.text
                        "The arithmetic rule can be used whenever there is a number on the left and right side, but not a variable, because it does not know anything about variables."
                  ]
            ]
      , start = Binary EqualTo (Atom (Num Nat.two)) (Atom (Num Nat.two))
      , finish = Atom Top
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Arithmetic relations which are false"
      , notes =
            [ Html.p
                  []
                  [ Html.text
                        "The arithmetic rule can also evaluate false relations. It even performs arithmetic as needed. The symbol for false is "
                  , Render.termSpan (Atom Bot)
                  , Html.text "."
                  ]
            ]
      , start = Binary EqualTo (Binary Addition (Atom (Num Nat.two)) (Atom (Num Nat.two))) (Atom (Num Nat.five))
      , finish = Atom Bot
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Reflexivity"
      , notes =
            [ Html.p
                  []
                  [ Html.text
                        "If an equation has the same expression on both sides, and that expression stands for a number (does not contain division by zero), the equation is always true. This is known as "
                  , Html.em [] [ Html.text "reflexivity" ]
                  , Html.text "."
                  ]
            , Html.p [] [ Render.leftRuleDiv reflexivityForEqualToRule ]
            , Html.p [] [ Html.text "Unlike the arithmetic rule, this works on expressions containing variables." ]
            ]
      , start =
            Binary
                EqualTo
                (Binary Multiplication (Atom (Num (Nat.fromInt 2))) (Atom AlgX))
                (Binary Multiplication (Atom (Num (Nat.fromInt 2))) (Atom AlgX))
      , finish = Atom Top
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Algebraic identity"
      , notes =
            [ Html.p
                  []
                  [ Html.text "An "
                  , Html.em [] [ Html.text "algebraic identity" ]
                  , Html.text
                        " is an algebraic equation which is always true. That is, true no matter what numbers are substituted for the variables. In this system, you can prove an algebraic identity by starting with an equation and ending with "
                  , Render.termSpan (Atom Top)
                  , Html.text "."
                  ]
            ]
      , start =
            Binary
                EqualTo
                (Binary Addition (Atom AlgA) (Atom AlgA))
                (Binary Multiplication (Atom (Num (Nat.fromInt 2))) (Atom AlgA))
      , finish = Atom Top
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            ]
      , scratch = allSymbols
      }
    , { description = "Conditional algebraic identity"
      , notes =
            [ Html.p
                  []
                  [ Html.text "A "
                  , Html.em [] [ Html.text "conditional algebraic identity" ]
                  , Html.text " is an equation which is always true, but only if a condition is met."
                  ]
            , Html.p
                  []
                  [ Html.text "This identity is always true when the common denominator "
                  , Render.termSpan (Atom AlgC)
                  , Html.text " is not equal to "
                  , Render.termSpan (Atom (Num Nat.zero))
                  , Html.text "."
                  ]
            , Html.p
                  []
                  [ Html.text "Conditions can be removed using the rule "
                  , Html.em [] [ Html.text "right annihilation for implication" ]
                  , Html.text "."
                  ]
            , Html.p [] [ Render.leftRuleDiv rightAnnihilationForImplication ]
            , Html.p
                  []
                  [ Html.text
                        "This rule uses an upper-case metavariable. Upper-case metavariables match logical expressions such as equations or "
                  , Render.termSpan (Atom Top)
                  , Html.text ", while lower-case metavariables match algebraic expressions."
                  ]
            , Html.p
                  []
                  [ Html.text "One way to think about this rule is: "
                  , Render.termSpan (Atom Top)
                  , Html.text " is true under any conditions, so conditions may be added or removed freely."
                  ]
            ]
      , start =
            Binary
                Implication
                (Binary NotEqualTo (Atom AlgC) (Atom (Num Nat.zero)))
                (Binary
                     EqualTo
                     (Binary Addition (Binary Division (Atom AlgA) (Atom AlgC)) (Binary Division (Atom AlgB) (Atom AlgC)))
                     (Binary Division (Binary Addition (Atom AlgA) (Atom AlgB)) (Atom AlgC))
                )
      , finish = Atom Top
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            , rightAnnihilationForImplication
            ]
      , scratch = allSymbols
      }
    , { description = "Annihilation"
      , notes =
            [ Html.p
                  []
                  [ Html.text "You may find the next "
                  , Render.termSpan (Atom (Num (Nat.fromInt 3)))
                  , Html.text
                        " problems quite difficult. Do not be suprised if you must spend hours or days trying to solve them, reviewing the rules to see how each one might help you."
                  ]
            , Html.p
                  []
                  [ Html.text "Zero times any number is zero. This property of zero is known as "
                  , Html.em [] [ Html.text "annihilation" ]
                  , Html.text "."
                  ]
            , Html.p [] [ Render.leftRuleDiv annihilationForMultiplicationRule ]
            , Html.p
                  []
                  [ Html.text "In this problem you will demonstrate that annihilation can be performed using the existing rules."
                  ]
            , Html.p [] [ Html.text "After this problem, you will be given the rule to use directly." ]
            ]
      , start = Binary EqualTo (Binary Multiplication (Atom (Num Nat.zero)) (Atom AlgX)) (Atom (Num Nat.zero))
      , finish = Atom Top
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            , rightAnnihilationForImplication
            ]
      , scratch = allSymbols
      }
    , { description = "Negation in terms of negative one"
      , notes =
            [ Html.p [] [ Html.text "It is conventient to replace negation with multiplication by negative one." ]
            , Html.p [] [ Render.leftRuleDiv negativeIdentityForMultiplicationRule ]
            , Html.p
                  []
                  [ Html.text "As with the previous problem, you will be asked to demonstrate this using the existing rules."
                  ]
            , Html.p [] [ Html.text "After this problem, you will be given a rule which performs this directly." ]
            ]
      , start = Binary EqualTo (Unary Neg (Atom AlgA)) (Binary Multiplication (Unary Neg (Atom (Num Nat.one))) (Atom AlgA))
      , finish = Atom Top
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , annihilationForMultiplicationRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            , rightAnnihilationForImplication
            ]
      , scratch = allSymbols
      }
    , { description = "Multiplication of inverses"
      , notes =
            [ Html.p
                  []
                  [ Html.text "This problem has two condition for the two denominators. It can be read aloud: “If "
                  , Render.termSpan (Atom AlgA)
                  , Html.text " is not equal to zero, then if "
                  , Render.termSpan (Atom AlgB)
                  , Html.text " is not equal to zero, then the inverse of "
                  , Render.termSpan (Atom AlgA)
                  , Html.text " times the inverse of "
                  , Render.termSpan (Atom AlgB)
                  , Html.text " is equal to the inverse of "
                  , Render.termSpan (Binary Multiplication (Atom AlgA) (Atom AlgB))
                  , Html.text ".”"
                  ]
            , Html.p [] [ Html.text "Remember, you can modify conditions to satisfy rules." ]
            , Html.p [] [ Html.text "After this problem, you will be given multiplication of inverses to use directly." ]
            , Html.p
                  []
                  [ Html.text
                        "All the traditional rules of basic algebra on single equations as well as the standard derived rules have now been covered. The remaining problems involve working with multiple equations."
                  ]
            ]
      , start =
            Binary
                Implication
                (Binary NotEqualTo (Atom AlgA) (Atom (Num Nat.zero)))
                (Binary
                     Implication
                     (Binary NotEqualTo (Atom AlgB) (Atom (Num Nat.zero)))
                     (Binary
                          EqualTo
                          (Binary
                               Multiplication
                               (Binary Division (Atom (Num Nat.one)) (Atom AlgA))
                               (Binary Division (Atom (Num Nat.one)) (Atom AlgB))
                          )
                          (Binary Division (Atom (Num Nat.one)) (Binary Multiplication (Atom AlgA) (Atom AlgB)))
                     )
                )
      , finish = Atom Top
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , annihilationForMultiplicationRule
            , negativeIdentityForMultiplicationRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            , rightAnnihilationForImplication
            ]
      , scratch = allSymbols
      }
    , { description = "Zero product rule"
      , notes =
            [ Html.p [] [ Html.text "Here is the zero product rule." ]
            , Html.p [] [ Render.leftRuleDiv zeroProductRule ]
            , Html.p
                  []
                  [ Html.text "You may not have seen the "
                  , Render.disjunctionHtml
                  , Html.text " symbol before. It is called the "
                  , Html.em [] [ Html.text "disjunction" ]
                  , Html.text " symbol or "
                  , Html.em [] [ Html.text "logical-or" ]
                  , Html.text
                        ". It is rarely seen in textbooks, but is common in computer proof systems such as this. It is usually read aloud as “or”."
                  ]
            , Html.p
                  []
                  [ Html.text
                        "In a disjunction, either the left-hand side is true, right-hand side is true, or both sides are true. So, in the lower formula, either "
                  , Render.termSpan (Binary EqualTo (Atom AlgA) (Atom (Num Nat.zero)))
                  , Html.text ", "
                  , Render.termSpan (Binary EqualTo (Atom AlgB) (Atom (Num Nat.zero)))
                  , Html.text ", or both."
                  ]
            ]
      , start = Binary EqualTo (Binary Multiplication (Atom AlgA) (Atom AlgB)) (Atom (Num Nat.zero))
      , finish =
            Binary
                Disjunction
                (Binary EqualTo (Atom AlgA) (Atom (Num Nat.zero)))
                (Binary EqualTo (Atom AlgB) (Atom (Num Nat.zero)))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , annihilationForMultiplicationRule
            , negativeIdentityForMultiplicationRule
            , multiplicationOfInversesRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , zeroProductRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            , rightAnnihilationForImplication
            ]
      , scratch = allSymbols
      }
    , { description = "Solving factors"
      , notes =
            [ Html.p
                  []
                  [ Html.text
                        "If you've studied factoring polynomial equations, the zero product rule is what let's you use the factors to find solutions."
                  ]
            ]
      , start =
            Binary
                EqualTo
                (Binary
                     Multiplication
                     (Binary Addition (Atom AlgX) (Atom (Num Nat.two)))
                     (Binary Addition (Atom AlgX) (Atom (Num Nat.three)))
                )
                (Atom (Num Nat.zero))
      , finish =
            Binary
                Disjunction
                (Binary EqualTo (Atom AlgX) (Unary Neg (Atom (Num Nat.two))))
                (Binary EqualTo (Atom AlgX) (Unary Neg (Atom (Num Nat.three))))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , annihilationForMultiplicationRule
            , negativeIdentityForMultiplicationRule
            , multiplicationOfInversesRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , zeroProductRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            , rightAnnihilationForImplication
            ]
      , scratch = allSymbols
      }
    , { description = "Identity for disjunction"
      , notes =
            [ Html.p [] [ Html.text "Disjunction has an identity rule, just like additon or multiplication." ]
            , Html.p [] [ Render.leftRuleDiv identityForDisjunctionRule ]
            , Html.p [] [ Html.text "In this case, false is the identity element." ]
            , Html.p
                  []
                  [ Html.text
                        "Try solving this problem by first using the zero product rule and simplifying. If you want, you can try solving the problem again without using the zero product rule."
                  ]
            ]
      , start =
            Binary
                EqualTo
                (Binary Multiplication (Binary Addition (Atom AlgX) (Atom (Num Nat.four))) (Atom (Num Nat.five)))
                (Atom (Num Nat.zero))
      , finish = Binary EqualTo (Atom AlgX) (Unary Neg (Atom (Num Nat.four)))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , annihilationForMultiplicationRule
            , negativeIdentityForMultiplicationRule
            , multiplicationOfInversesRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , zeroProductRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            , rightAnnihilationForImplication
            , identityForDisjunctionRule
            ]
      , scratch = allSymbols
      }
    , { description = "Annihilation for disjunction"
      , notes =
            [ Html.p [] [ Html.text "Disjunction also has an annihilation rule. True is the annihilation element." ]
            , Html.p [] [ Render.leftRuleDiv annihilationForDisjunctionRule ]
            , Html.p
                  []
                  [ Html.text "Again, this problem can be solved both by using the zero product rule, and without it."
                  ]
            ]
      , start =
            Binary
                EqualTo
                (Binary Multiplication (Atom (Num Nat.zero)) (Binary Addition (Atom AlgX) (Atom (Num Nat.three))))
                (Atom (Num Nat.zero))
      , finish = Atom Top
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , annihilationForMultiplicationRule
            , negativeIdentityForMultiplicationRule
            , multiplicationOfInversesRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , zeroProductRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            , rightAnnihilationForImplication
            , identityForDisjunctionRule
            , annihilationForDisjunctionRule
            ]
      , scratch = allSymbols
      }
    , { description = "Commutativity for disjunction"
      , notes =
            [ Html.p
                  []
                  [ Html.text
                        "Commutativity for disjunction looks nearly the same as the commutativity rules for addition and multiplication."
                  ]
            , Html.p [] [ Render.leftRuleDiv commutativityForDisjunctionRule ]
            ]
      , start =
            Binary
                EqualTo
                (Binary
                     Multiplication
                     (Binary Addition (Atom AlgX) (Atom (Num Nat.three)))
                     (Binary Addition (Atom AlgX) (Atom (Num Nat.five)))
                )
                (Atom (Num Nat.zero))
      , finish =
            Binary
                Disjunction
                (Binary EqualTo (Atom AlgX) (Unary Neg (Atom (Num Nat.five))))
                (Binary EqualTo (Atom AlgX) (Unary Neg (Atom (Num Nat.three))))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , annihilationForMultiplicationRule
            , negativeIdentityForMultiplicationRule
            , multiplicationOfInversesRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , zeroProductRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            , rightAnnihilationForImplication
            , identityForDisjunctionRule
            , annihilationForDisjunctionRule
            , commutativityForDisjunctionRule
            ]
      , scratch = allSymbols
      }
    , { description = "Idempotency for disjunction"
      , notes =
            [ Html.p
                  []
                  [ Html.text
                        "I'll introduce two new rules to help solve this problem. First, a simple rule to replace squaring with multiplication."
                  ]
            , Html.p [] [ Render.leftRuleDiv squareAsProductRule ]
            , Html.p [] [ Html.text "Next, idempotency for disjunction." ]
            , Html.p [] [ Render.leftRuleDiv idempotencyForDisjunctionRule ]
            , Html.p [] [ Html.text "This rule let's you remove duplicates." ]
            ]
      , start = Binary EqualTo (Unary Square (Atom AlgX)) (Atom (Num Nat.zero))
      , finish = Binary EqualTo (Atom AlgX) (Atom (Num Nat.zero))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , annihilationForMultiplicationRule
            , negativeIdentityForMultiplicationRule
            , multiplicationOfInversesRule
            , squareAsProductRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , zeroProductRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            , rightAnnihilationForImplication
            , identityForDisjunctionRule
            , annihilationForDisjunctionRule
            , idempotencyForDisjunctionRule
            , commutativityForDisjunctionRule
            ]
      , scratch = allSymbols
      }
    , { description = "Product of squares"
      , notes = [ Html.p [] [ Html.text "Let's prove some algebraic identities having to do with squares." ] ]
      , start =
            Binary
                EqualTo
                (Binary Multiplication (Unary Square (Atom AlgA)) (Unary Square (Atom AlgB)))
                (Unary Square (Binary Multiplication (Atom AlgA) (Atom AlgB)))
      , finish = Atom Top
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , annihilationForMultiplicationRule
            , negativeIdentityForMultiplicationRule
            , multiplicationOfInversesRule
            , squareAsProductRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , zeroProductRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            , rightAnnihilationForImplication
            , identityForDisjunctionRule
            , annihilationForDisjunctionRule
            , idempotencyForDisjunctionRule
            , commutativityForDisjunctionRule
            ]
      , scratch = allSymbols
      }
    , { description = "Square of negation"
      , notes =
            [ Html.p
                  []
                  [ Html.text "Another useful algebraic identity. The property that squaring removes negation is known as "
                  , Html.em [] [ Html.text "evenness" ]
                  , Html.text ". So named because all even exponents have this property."
                  ]
            ]
      , start = Binary EqualTo (Unary Square (Unary Neg (Atom AlgA))) (Unary Square (Atom AlgA))
      , finish = Atom Top
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , annihilationForMultiplicationRule
            , negativeIdentityForMultiplicationRule
            , multiplicationOfInversesRule
            , squareAsProductRule
            , productOfSquaresRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , zeroProductRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            , rightAnnihilationForImplication
            , identityForDisjunctionRule
            , annihilationForDisjunctionRule
            , idempotencyForDisjunctionRule
            , commutativityForDisjunctionRule
            ]
      , scratch = allSymbols
      }
    , { description = "Cancellation of squares"
      , notes =
            [ Html.p
                  []
                  [ Html.text
                        "It might be useful to work this problem out on paper backwards. That is, see if you can start with the goal formula and use the zero product rule in reverse."
                  ]
            , Html.p [] [ Html.text "After this problem, I'll add cancellation of squares to your list of rules." ]
            ]
      , start = Binary EqualTo (Unary Square (Atom AlgX)) (Unary Square (Atom AlgA))
      , finish =
            Binary
                Disjunction
                (Binary EqualTo (Atom AlgX) (Atom AlgA))
                (Binary EqualTo (Atom AlgX) (Unary Neg (Atom AlgA)))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , annihilationForMultiplicationRule
            , negativeIdentityForMultiplicationRule
            , multiplicationOfInversesRule
            , squareAsProductRule
            , productOfSquaresRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , zeroProductRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            , rightAnnihilationForImplication
            , identityForDisjunctionRule
            , annihilationForDisjunctionRule
            , idempotencyForDisjunctionRule
            , commutativityForDisjunctionRule
            ]
      , scratch = allSymbols
      }
    , { description = "Square root of 4"
      , notes =
            [ Html.p
                  []
                  [ Html.text
                        "The rule cancellation of squares should come in handy here. Remember to use the scratch pad to create compound expressions."
                  ]
            ]
      , start = Binary EqualTo (Unary Square (Atom AlgX)) (Atom (Num (Nat.fromInt 4)))
      , finish =
            Binary
                Disjunction
                (Binary EqualTo (Atom AlgX) (Atom (Num Nat.two)))
                (Binary EqualTo (Atom AlgX) (Unary Neg (Atom (Num Nat.two))))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , annihilationForMultiplicationRule
            , negativeIdentityForMultiplicationRule
            , multiplicationOfInversesRule
            , squareAsProductRule
            , productOfSquaresRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , zeroProductRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , cancellationOfSquaresRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            , rightAnnihilationForImplication
            , identityForDisjunctionRule
            , annihilationForDisjunctionRule
            , idempotencyForDisjunctionRule
            , commutativityForDisjunctionRule
            ]
      , scratch = allSymbols
      }
    , { description = "Conjunction and substitution"
      , notes =
            [ Html.p
                  []
                  [ Html.text "The "
                  , Render.conjunctionHtml
                  , Html.text " symbol is called the "
                  , Html.em [] [ Html.text "conjunction" ]
                  , Html.text " symbol or "
                  , Html.em [] [ Html.text "logical-and" ]
                  , Html.text ". In a conjunction, both sides must be true. It is usually read aloud as “and”."
                  ]
            , Html.p
                  []
                  [ Html.text
                        "If both sides of a conjunction are equations for lines, solving the formula amounts to determining where the lines intersect."
                  ]
            , Html.p
                  []
                  [ Html.text "Conjunctions come with a powerful rule called "
                  , Html.em [] [ Html.text "substitution" ]
                  , Html.text "."
                  ]
            , Html.p [] [ Render.leftRuleDiv substitutionForEqualToRule ]
            , Html.p
                  []
                  [ Html.text
                        "When you are on the right-hand side of a conjunction, you can use the equation on the left-hand side for substitution and vice versa. Try substituting the "
                  , Render.termSpan (Atom AlgX)
                  , Html.text " on the right-hand side to solve the problem."
                  ]
            ]
      , start =
            Binary
                Conjunction
                (Binary EqualTo (Atom AlgX) (Atom (Num (Nat.fromInt 2))))
                (Binary EqualTo (Atom AlgY) (Binary Addition (Atom AlgX) (Atom (Num Nat.one))))
      , finish =
            Binary
                Conjunction
                (Binary EqualTo (Atom AlgX) (Atom (Num (Nat.fromInt 2))))
                (Binary EqualTo (Atom AlgY) (Atom (Num (Nat.fromInt 3))))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , annihilationForMultiplicationRule
            , negativeIdentityForMultiplicationRule
            , multiplicationOfInversesRule
            , squareAsProductRule
            , productOfSquaresRule
            , substitutionForEqualToRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , zeroProductRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , cancellationOfSquaresRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            , rightAnnihilationForImplication
            , identityForDisjunctionRule
            , annihilationForDisjunctionRule
            , idempotencyForDisjunctionRule
            , commutativityForDisjunctionRule
            ]
      , scratch = allSymbols
      }
    , { description = "Solving systems of equations"
      , notes =
            [ Html.p
                  []
                  [ Html.text
                        "This problem will require many of tactics you have learned. If you have difficulty you may want to go back and revisit some of the provious problems."
                  ]
            , Html.p
                  []
                  [ Html.text
                        "I'll add all the rules for conjunctions rather than go through them one by one, as you've seen the corresponding rules for disjunction."
                  ]
            , Html.p [] [ Render.leftRuleDiv identityForConjunctionRule ]
            , Html.p [] [ Render.leftRuleDiv annihilationForConjunctionRule ]
            , Html.p [] [ Render.leftRuleDiv commutativityForConjunctionRule ]
            , Html.p [] [ Render.leftRuleDiv idempotencyForConjunctionRule ]
            ]
      , start =
            Binary
                Conjunction
                (Binary EqualTo (Binary Addition (Atom AlgX) (Atom AlgY)) (Atom (Num (Nat.fromInt 8))))
                (Binary EqualTo (Binary Subtraction (Atom AlgX) (Atom AlgY)) (Atom (Num (Nat.fromInt 2))))
      , finish =
            Binary
                Conjunction
                (Binary EqualTo (Atom AlgX) (Atom (Num (Nat.fromInt 5))))
                (Binary EqualTo (Atom AlgY) (Atom (Num (Nat.fromInt 3))))
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , annihilationForMultiplicationRule
            , negativeIdentityForMultiplicationRule
            , multiplicationOfInversesRule
            , squareAsProductRule
            , productOfSquaresRule
            , substitutionForEqualToRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , zeroProductRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , cancellationOfSquaresRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            , rightAnnihilationForImplication
            , identityForConjunctionRule
            , identityForDisjunctionRule
            , annihilationForConjunctionRule
            , annihilationForDisjunctionRule
            , idempotencyForConjunctionRule
            , idempotencyForDisjunctionRule
            , commutativityForConjunctionRule
            , commutativityForDisjunctionRule
            ]
      , scratch = allSymbols
      }
    , { description = "Intersection of a line with a parabola"
      , notes =
            [ Html.p
                  []
                  [ Html.text
                        "This problem is asking you to find the intersection points of an equation for a parabola and an equation for a line."
                  ]
            , Html.p
                  []
                  [ Html.text
                        "Here are two more rules to use. Conjunction distributes over disjunction and disjunction distributes over conjunction."
                  ]
            , Html.p [] [ Render.leftRuleDiv distributionOfConjunctionOverDisjunction ]
            , Html.p [] [ Render.leftRuleDiv distributionOfDisjunctionOverConjunction ]
            , Html.p [] [ Html.text "There's a lot to do here. It should take twenty to thirty steps to solve." ]
            ]
      , start =
            Binary
                Conjunction
                (Binary EqualTo (Atom AlgY) (Binary Addition (Unary Square (Atom AlgX)) (Atom (Num Nat.one))))
                (Binary
                     EqualTo
                     (Atom AlgY)
                     (Binary Addition (Binary Multiplication (Atom (Num Nat.two)) (Atom AlgX)) (Atom (Num Nat.one)))
                )
      , finish =
            Binary
                Disjunction
                (Binary
                     Conjunction
                     (Binary EqualTo (Atom AlgX) (Atom (Num Nat.zero)))
                     (Binary EqualTo (Atom AlgY) (Atom (Num Nat.one)))
                )
                (Binary
                     Conjunction
                     (Binary EqualTo (Atom AlgX) (Atom (Num Nat.two)))
                     (Binary EqualTo (Atom AlgY) (Atom (Num Nat.five)))
                )
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , annihilationForMultiplicationRule
            , negativeIdentityForMultiplicationRule
            , multiplicationOfInversesRule
            , squareAsProductRule
            , productOfSquaresRule
            , substitutionForEqualToRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , zeroProductRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , cancellationOfSquaresRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            , rightAnnihilationForImplication
            , identityForConjunctionRule
            , identityForDisjunctionRule
            , annihilationForConjunctionRule
            , annihilationForDisjunctionRule
            , idempotencyForConjunctionRule
            , idempotencyForDisjunctionRule
            , commutativityForConjunctionRule
            , commutativityForDisjunctionRule
            , distributionOfConjunctionOverDisjunction
            , distributionOfDisjunctionOverConjunction
            ]
      , scratch = allSymbols
      }
    , { description = "Contradiction"
      , notes =
            [ Html.p
                  []
                  [ Html.text
                        "Let's end by proving a contradiction. It is impossible (i.e. false) for a number to be both equal to zero and not equal to zero."
                  ]
            , Html.p [] [ Html.text "Here are two rules which may be useful." ]
            , Html.p [] [ Render.leftRuleDiv negationOfTrueRule ]
            , Html.p [] [ Render.leftRuleDiv negationOfFalseRule ]
            , Html.p
                  []
                  [ Html.text "These are rather simple. The negation of true is false and the negation of false is true."
                  ]
            ]
      , start =
            Binary
                Conjunction
                (Binary EqualTo (Atom AlgA) (Atom (Num Nat.zero)))
                (Binary NotEqualTo (Atom AlgA) (Atom (Num Nat.zero)))
      , finish = Atom Bot
      , rules =
            [ arithmeticRule
            , identityForAdditionRule
            , identityForMultiplicationRule
            , commutativityForAdditionRule
            , commutativityForMultiplicationRule
            , associativityForAdditionRule
            , associativityForMultiplicationRule
            , distributionOfMultiplicationOverAdditionRule
            , inverseForAdditionRule
            , inverseForMultiplicationRule
            , additionOfNegationRule
            , multiplicationOfInverseRule
            , annihilationForMultiplicationRule
            , negativeIdentityForMultiplicationRule
            , multiplicationOfInversesRule
            , squareAsProductRule
            , productOfSquaresRule
            , substitutionForEqualToRule
            , reflexivityForEqualToRule
            , symmetryForEqualToRule
            , notEqualToInTermsOfLogicalNegationRule
            , zeroProductRule
            , additionForEqualToRule
            , multiplicationForEqualToRule
            , cancellationOfSquaresRule
            , subtractionForEqualToRule
            , divisionForEqualToRule
            , negationOfTrueRule
            , negationOfFalseRule
            , rightAnnihilationForImplication
            , identityForConjunctionRule
            , identityForDisjunctionRule
            , annihilationForConjunctionRule
            , annihilationForDisjunctionRule
            , idempotencyForConjunctionRule
            , idempotencyForDisjunctionRule
            , commutativityForConjunctionRule
            , commutativityForDisjunctionRule
            , distributionOfConjunctionOverDisjunction
            , distributionOfDisjunctionOverConjunction
            ]
      , scratch = allSymbols
      }
    ]
