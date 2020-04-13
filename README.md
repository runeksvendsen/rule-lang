# A domain-specific language for portfolio compliance rules

## Introduction

### Scope

The scope of the project is to design, and implement in Haskell, a domain-specific language (DSL) for expressing portfolio compliance rules. The basis for this DSL is the *Comprehension syntax*-language [1]. In addition, the compliance rules that the DSL must support is restricted to the **six** rules listed in [Compliance rule examples](#compliance-rule-examples).

## Background

### Portfolio compliance

Companies/institutions that invest money on behalf of customers — hereafter referred to as *institutional investors* — are subject to laws that govern what they are allowed to invest in. For example, a law might require that a pension fund invest no more than a certain fraction of its funds in the bonds of companies belonging to a single business sector — e.g. *the value of bonds from companies in agriculture may not exceed 5% of the value of total assets*. A large number of these *portfolio compliance rules* apply to institutional investors, who must keep track of whether or not their investments comply with these rules.

#### Terms

##### Security

The term *security* refers to any asset that an institutional investor can be in possession of. This includes, but is not limited to: cash (money in a bank account), bonds (long term debt obligations), company stock (company ownership), money market instruments (short term debt obligations). A single security — e.g. one share of IBM stock — is the smallest unit described in this paper.

##### Position

The term *position* refers to one or more of the same type of security. For example, the ownership of five shares of Microsoft stock at a current market value of USD 150 each comprises a *position* in Microsoft stock with a current market value of USD 750. The position is the smallest unit that a portfolio compliance rule can apply to. Thus, no portfolio compliance rule distinguishes between owning e.g. ten shares of Google with a value of USD 1000 each versus eight shares of Google with a value of USD 1250 each — both comprise a position in Google shares with a value of USD 10000.

##### Portfolio

The term *portfolio* refers to a *set of positions*. A particular portfolio may contain positions of the same *type* from the same *region*, e.g. Asian stocks; it may contain all the positions of a particular *client* (a given customer of the institutional investor); or it may contain all positions governed by a particular portfolio compliance rule. For the purpose of this paper, the latter is assumed. That is, a portfolio — containing a number of positions — exists because the positions herein are governed by the same compliance rule(s).

#### Compliance rule examples

In this section six different compliance rules are presented (REF: [David's rules](#a.1)). Common to all rules is that a position has one or more properties. A property is identified by a name — such as *value*, *issuer*, and *security type* — and an associated value.

##### Rule I

*Maximum 10% of assets in any single issuer. Positions with the same issuer whose value is greater than 5% of total assets may not in aggregate exceed 40% of total assets.*

This rule is composed of two sub-rules. In both cases we begin by separating the positions into groups, such that each group contains only positions that have the same issuer (hereafter referred to as *grouping by issuer*). After this initial step, the two sub-rules proceed as follows:

1. For all the groups: the value of a group must be at most 10% of the total portfolio value.

2. Remove all of the groups whose value relative to the portfolio is less than or equal to 5%. The total aggregate value of the remaining groups must be at most 40% of the total portfolio value.

##### Rule II

*No more than 35% in securities of the same issuer, and if more than 30% in one then issuer must be made up of at least 6 different issues.* 

This rule is also composed of two sub-rules. As in the previous rule, the first step for both sub-rules is to group by issuer.

1. For all the groups: the value of a group is less than or equal to 35% of the total portfolio value.
2. For all groups whose relative value is greater than 30%: a *count* of the number of different *issues* must be greater than or equal to *six* (where counting the number of different issues in a group amounts to grouping by issuer and counting the number of resulting groups).

##### Rule III

*When holding >35% in a single issuer of government and public securities, then there can be no more than 30% in any single issue, and a minimum of 6 different issues is required.*

The first step is to filter off anything that is *not* either a government or public security. Next we group by issuer, followed by:

1. For all the *issuer*-groups: only if the group value is greater than 35% of the total portfolio value: then group by *issue* and proceed to **2.**
2. a. The *issue*-group count must be at least 6, and
   b. For all *issue*-groups: the value of the *issue*-group must be less than or equal to 30% of total portfolio value

##### Rule IV

*The risk exposure to a counter-party to an OTC derivative may not exceed 5% NA; the limit is raised to 10% for approved credit institutions.*

**TODO:** elaborate

##### Rule V

*Max 5% in any single security rated better than BBB: otherwise the maximum is 1% of fund net value.*

**TODO:** elaborate

##### Rule VI

*The portfolio shall invest in minimum 5 / 4 / 3 / 2 different foreign countries if: aggregate value of foreign countries relative to portfolio >=80% / >=60% / >=40% / <40%, respectively.*

**TODO:** elaborate

# References

[1] Peter Buneman, Leonid Libkin, Dan Suciu, Val Tannen, and Limsoon Wong. 1994. *Comprehension syntax*. SIGMOD Rec. 23, 1 (March 1994), 87–96. DOI:https://doi.org/10.1145/181550.181564
