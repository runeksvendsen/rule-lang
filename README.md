# Introduction

Companies/institutions that invest money on behalf of customers — hereafter referred to as *institutional investors* — are subject to laws that govern what they are allowed to invest in. For example — in order to reduce the risk of monetary loss for pension holders — a law might require that a pension fund invest no more than a certain fraction of its funds in a single business sector (e.g. *agriculture*, *manufacturing*, or *construction*). A law of this nature is called a *portfolio compliance rule* because it defines a rule that a given *portfolio* (the set of investments owned by the institutional investor) must comply with. A large number of portfolio compliance rules apply to institutional investors, who must keep track of whether or not their investments are in compliance with these rules.

SimCorp is a *financial software*-company, whose customers include asset managers, banks, national banks, pension funds, sovereign wealth funds and insurance companies[@SimCorpFactSheet]. Their core software product *SimCorp Dimension* includes — among many other features — the ability to automatically check portfolios against a set of compliance rules. This enables institutional investors to spend less time dealing with portfolio compliance rules, as computer software can automatically perform this task of checking portfolios for compliance with rules and regulations.

Overall, SimCorp's current software solution for portfolio compliance rules works well, but there is room for improvement:

* From a usability point-of-view, some users prefer being able to type in rules using the keyboard — including e.g. auto-completion. SimCorp's current solution does not easily allow for adding this.
* When writing compliance rules using SimCorp's software, the rule author can make use of so-called *rule fragments*. A rule fragment is a small, re-usable rule condition. By combining together many of these  conditions, complex compliance rules can be created without having to write the entire rule from scratch.
  * SimCorp's intention is to promote the reuse of rule fragments across compliance rules. In reality however, only few fragments are used in more than one rule. 
  * Conceptually, fragments are very powerful, but also difficult to master. They allow for many logical constructions that do not make sense in a business context. SimCorp would like a more user-friendly solution with a less steep learning curve.


# Scope

The scope of this project is to design, and implement in Haskell, a domain-specific language (DSL) for expressing portfolio compliance rules. The DSL must support at least the six rules listed in [Compliance rule examples](#compliance-rule-examples), which are representative of the more complex rules in use, but we do not strive to cover all rules used in practice. In addition, the DSL will be based on the concept of *purely functional programming*, with the aim of reaping the many advantages of this programming paradigm (see @sec:functional-advantages)

# Background

## Portfolio compliance

**TODO:** *anything to add here that isn't in the Introduction?*

### Terminology

#### Security

The term *security* refers to any asset that an institutional investor can be in possession of. This includes, but is not limited to: cash (money in a bank account), bonds (long term debt obligations), company stock (company ownership), money market instruments (short term debt obligations). A single security — e.g. one share of IBM stock — is the smallest unit described in this paper.

#### Position

The term *position* refers to one or more of the same type of security. For example, the ownership of five shares of Microsoft stock at a current market value of USD 150 each comprises a *position* in Microsoft stock with a current market value of USD 750. The position is the smallest unit that a portfolio compliance rule can apply to. Thus, no portfolio compliance rule distinguishes between owning e.g. ten shares of Google with a value of USD 1000 each versus eight shares of Google with a value of USD 1250 each — both comprise a position in Google shares with a value of USD 10000.

#### Portfolio

The term *portfolio* refers to a *set of positions*. A particular portfolio may contain positions of the same *type* from the same *region*, e.g. Asian stocks; it may contain all the positions of a particular *client* (a given customer of the institutional investor); or it may contain all positions governed by a particular portfolio compliance rule. For the purpose of this paper, the latter is assumed. That is, a portfolio — containing a number of positions — exists because the positions herein are governed by the same compliance rule(s).

### Compliance rule examples

In this section, six different compliance rules are presented. Common to all rules is that a position has one or more properties. A property is identified by a name — e.g. *value*, *issuer*, or *security type* — and an associated value — e.g., respectively, $1000.50$, `"Tesla"`, and `"Bond"`.

#### Rule I

*Maximum 10% of assets in any single issuer. Positions with the same issuer whose value is greater than 5% of total assets may not in aggregate exceed 40% of total assets.*

This rule is composed of two sub-rules. In both cases we begin by separating the positions into groups, such that each group contains only positions that have the same issuer (hereafter referred to as *grouping by issuer*). After this initial step, the two sub-rules proceed as follows:

1. For each group: the value of the group must be at most 10% of the total portfolio value.

2. Remove all of the groups whose value relative to the portfolio is less than or equal to 5%. The total aggregate value of the remaining groups must be at most 40% of the total portfolio value.

#### Rule II

*No more than 35% in securities of the same issuer, and if more than 30% in one then issuer must be made up of at least 6 different issues.* 

This rule is also composed of two sub-rules. As in the previous rule, the first step for both sub-rules is to group by issuer.

1. For each group: the value of the group is less than or equal to 35% of the total portfolio value.
2. For each group whose relative value is greater than 30%: a *count* of the number of different *issues* must be greater than or equal to *six* (where counting the number of different issues in a group amounts to grouping by issuer and counting the number of resulting groups).

#### Rule III

*When holding >35% in a single issuer of government and public securities, then there can be no more than 30% in any single issue, and a minimum of 6 different issues is required.*

The first step is to filter off anything that is *not* either a government or public security. Next we group by issuer, followed by:

1. For each *issuer*-group: only if the group value is greater than 35% of the total portfolio value: then group by *issue* and proceed to **2.**
2. a. The *issue*-group count must be at least 6, and
   b. For each *issue*-group: the value of the *issue*-group must be less than or equal to 30% of total portfolio value

#### Rule IV

*The risk exposure to a counterparty to an OTC derivative may not exceed 5% NA; the limit is raised to 10% for approved credit institutions.*

First, filter off anything that is not an *OTC derivative*. Next, for each counterparty:

* If the counterparty **is not** an approved credit institution, then the exposure of the counterparty relative to the portfolio exposure must be at most 5%
* If the counterparty **is** an approved credit institution, then the exposure of the counterparty relative to the portfolio exposure must be at most 10%

#### Rule V

*Max 5% in any single security rated better than BBB: otherwise the maximum is 1% of fund net value.*

First, group by security, then:

* If the security's rating is **AAA** or **AA** or **A**, then the value of the position must be at most 5% relative to portfolio value
* If the rating is **BBB**, or **BB**, or **B**, or **CCC**, or **CC**, or **C**, or **D**, then the value of the position must be at most 1% relative to portfolio value

***Sprøgsmål til Peter:*** *de konstruktioner, som denne regel bruger, er identiske med IV. Bør jeg overveje at fjerne denne regel, da den ikke tilføjer noget til sproget?*

#### Rule VI

*The portfolio shall invest in minimum 5 / 4 / 3 / 2 different foreign countries if: aggregate value of foreign countries relative to portfolio >=80% / >=60% / >=40% / <40%, respectively.*

First, filter off domestic positions, in order to obtain only foreign-country positions. Next, calculate the value of foreign-country positions relative to the entire portfolio (including domestic positions), as well as the number of foreign countries. Then:

* If foreign-country value is at least **80%**: foreign-country count must be at least **5**
* If foreign-country value is at least **60%**: foreign-country count must be at least **4**
* If foreign-country value is at least **40%**: foreign-country count must be at least **3**
* If foreign-country value is less than **40%**: foreign-country count must be at least **2**

## Domain-specific language

A domain-specific language (DSL) is a programming language that is tailored to model a specific business domain. A DSL stands in contrast to a *general-purpose* programming language (GPPL), which is designed to model *any* business domain. A DSL is thus less expressive than a general-purpose languages, in the sense that a DSL intentionally restricts the domain that can be modelled using the language.

DSL examples include: HTML (*Hypertext Markup Language*) for describing the structure of a web page; CSS (*Cascading Style Sheets*) for describing the presentation of a web page (e.g., layout, colors, fonts); and SQL (*Structured Query Language*) for describing queries against a table-based database.

### Purpose

Due to the restriction in what a DSL must be capable of modeling, it is possible to design a DSL that is significantly simpler than a GPPL. And while this comes with the disadvantage of reducing what is possible to express using the DSL, it also comes with the advantage of a reduction in the time/effort needed to learn it. Consequently, if the goal is to get experts, of a particular business domain, to quickly/easily express their domain knowledge in code, which can be executed by a computer, a simple DSL can be a helpful tool.

### Terminology

#### *Abstract* versus *concrete* syntax

The *abstract syntax* of a programming language (whether domain-specific or general-purpose) is a data structure that describes any expression in that language. As an example, let us consider a very simple DSL that describes multiplication and addition of integers. This language has two *data constructors*: $Mul$ and $Add$, which describe multiplication and addition, respectively. Both of these data constructors take two arguments, which may be either an integer or another expression — i.e. either a multiplication or addition or a combination hereof. The abstract syntax $Add\;3\;5$ thus describes three added to five, $Mul\;2\;7$ describes two multiplied by seven, and $Mul\;4\;(Add\;1\;6)$ describes multiplying by four the result of adding one to six. This syntax is called "abstract" because it refers to abstract objects. The objects $Add$ and $Mul$ are abstract in the sense that $Add$ and $Mul$ are simply *names* that we use to refer to the abstract operation of addition and multiplication, respectively — we could just as well refer to these objects as $A$ and $M$.

The *concrete syntax* of a programming language is **a representation** of the abstract syntax. For example, a common representation of $Mul\;4\;(Add\;1\;6)$ — i.e. multiplying by four the result of adding one to six — is `4 * (1 + 6)`. But we may also refer to this same operation in concrete syntax by adding a (redundant) pair of parenthesis around each subexpression:  `(4) * ((1 + 6))` — both refer to exactly the same operation. Thus, as can be seen from this example, a single piece of abstract syntax can be represented in multiple ways using concrete syntax.

#### *Printer* versus *parser*

A *parser* converts concrete syntax into abstract syntax, while a *printer* converts abstract syntax into concrete syntax. A parser can fail because it can be given invalid input. Using the above example of multiplication and addition, a parser given the input `4×(1+6` will fail (because an ending parenthesis is missing). A printer cannot fail — it should be able to convert any abstract syntax into concrete syntax.

Given a parser and a printer for the same language, feeding to the parser the output of applying the printer to any piece of abstract syntax should yield the same piece of abstract syntax. The opposite, however, is not the case, as the printer cannot know e.g. how many redundant parentheses were in the original concrete syntax, and may thus output different concrete syntax.

#### *Embedded* DSL versus *external* DSL

An *external* DSL defines its own abstract and concrete syntax. An *embedded* DSL (also called an *internal* DSL) is written inside an existing GPPL (called the *host language*). 

**Spørgsmål til Peter:** kender du en god forklaring af essensen af et embedded DSL? Jeg har umiddelbart svært ved at definere hvad der kendetegner et embedded DSL sammenlignet med fx bare et bibliotek. Er grænsen veldefineret, eller er den relativt sløret?

### Comparison with GPPL

#### Benefits

* A restriction of the constructs available for recursion can guarantee termination. For example, restricting iteration to *"doing something for every item in a list"*, as well as not allowing infinite lists, guarantees that programs will terminate (i.e. not loop infinitely).
* Separation of the *language* from the *model described using the language* allows different interpretations of the same model:
  * *Formatting* the model for ease of readability, by printing out using syntax highlighting and standardized formatting
  * *Visualizing* the model, i.e. producing an image that represents the model
  * *Perform static checks* on the model, i.e. checking the model for inconsistencies and errors that may not be possible if the model were expressed in a GPPL
  * Different implementations of programs that evaluate the model — as opposed to being tied to the GPPL in which the model is formulated, e.g.:
    * One evaluator written in C for performance — while sacrificing memory safety (and thus risking security vulnerabilities)
    * Another evaluator written in Haskell for safety —  while sacrificing performance

#### Drawbacks

* User needs to learn a new language, including both syntax and semantics

## Purely functional programming

### Overview

* Applying and composing mathematical functions

* Functions can be bound to variables and given as argument to other functions. Example: *map*
* 

### Terminology

#### Mathematical function

* Mathematical functions: takes one or more input arguments and returns a value
  * *Purely functional programming:* a function given the same argument(s) returns the same result

#### Higher-order functions
* Takes a function as argument and/or returns a function
* 

#### Immutability
A mutable variable is a location — which may or may not contain a value — while an immutable variable is a *synonym*: giving a name to a concrete value. Java example:

```java
int i;
i = 0;
i = 27;
```

Line 1 *declares* a mutable variable, in other words: associates the *name* `i` with some location in memory. Line 2 stores the value `0` in the memory location referred to as `i`, and line 3 stores the value `27` in this same location in memory.

The above Java example has no parallel in purely functional programming. The Haskell example below bears *some* resemblance to the above example:

```haskell
i = 0
someFunction n = let i = 27 in n*i
anotherFunction n = n*i
```

Line 1 declares `i` as a synonym for the value `0`. Line 2 starts the definition of a function named `someFunction` which takes a single argument `n`. Line 3 declares *another* variabl… **TODO**

* A global, mutable variable makes functions (potentially) impure: changing a global variable and re-running a function with the same arguments might return a different value because the mutable variable was updated.

### Advantages {#sec:functional-advantages}

#### Composability {#sec:composability}
*Composability* is the ability to compose smaller fragments of code into larger ones.

* Functions as input/output-"pipe" results in composability: $g \circ f$

#### Parallel evaluation

* No side-effects: (sub-)expressions can be evaluated in parallel

### Disadvantages

* Speed
  * Lack of in-place updates (mutability)
    * E.g. array updates 
  * Garbage collection

