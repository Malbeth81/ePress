# ePress

ePress is an efficient and fully features templating engine that can be used in a variety of context.
 
Simply write your template and embed logic into it using bash-like `${}` tags.

Features:
* Supports both numeric & string types (with automatic type casting).
* Supports complex mathematical equations.
* Supports string concatenation.
* Supports static variable definitions.
* Supports simple and complex assertions.
* Supports simple and complex conditions.
* Supports simple and complex iterations.

Examples:
```
${}

${
    'Hello, ' +
    name + "!"
}

${var v1 = 1}
${var v2 = "true"}
${var v3 = 1}
${var v4 = 1}
${var v5 = 3 +
2}

${if v1 + 2 > 2 then "Condition 1 alpha" else if bar + 1 > 1 then "Condition 1 bravo" else "Condition 1 charlie"}

${if v2}
Condition 2 alpha
$else
Condition 2 bravo
$end

${  if v3 then "Condition " + "3" + " alpha" else }
Condition 3 bravo
$end

${if v4 + 100 > 100 }
Condition 4 alpha
${else if v4 < 0 }
Condition 4 bravo
${else "Condition 4 charlie"}

${if (2*(3+23))+2*3+1 <= 5*(9*(4+5*(2+1)))+1}
    ${for 1 to 4 do "Iteration 1-" + index + " "}
    ${for 1 to 4 do
        if index % 2 > 0 then "Iteration 2 odd " else "Iteration 2 even "}
    ${for 1 to 4 do}${if index > 1}, $end${"Iteration 3-" + index}$end
$else
    foo
    ${if v4 > 0 then "bar"}
$end

${  for v5  }
    ${if index % 2 > 0 then
        "Iteration 4 odd"
    else
        "Iteration 4 even"
    }
$end
```