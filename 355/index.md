---
title: CptS 355 — Programming Language Design
subtitle: Washington State University · Fall 2026
description: Syllabus, schedule, notes, and readings for CptS 355, Programming Language Design, at Washington State University, Fall 2026.
keywords: cpts 355, programming language design, wsu, washington state university, interpreters, semantics, python, racket, functional programming, thomas gilray
bg: fewcritters
---

<div class="page-people" markdown="1">

**Instructor:** Thomas Gilray {{email:thomas.gilray@wsu.edu}} — EME B49, office hours <span class="tbd">TBD</span>

**Teaching assistants:** Aidan Johnson {{email:aidan.johnson@wsu.edu}} and Joseph Buchholz {{email:joseph.buchholz@wsu.edu}} — office hours <span class="tbd">TBD</span>

**Lectures:** Tuesdays and Thursdays, 4:20–5:35pm, in [SEH 105](https://maps.google.com/?q=46.7288,-117.1682) — Schweitzer Engineering Hall, the new engineering building · 3 credits

</div>

CptS 355 is a tour of how programming languages are designed, and of the surprisingly small set of ideas that nearly all of them are assembled from. We will work across several paradigms — imperative and object-oriented scripting in Python 3, functional programming in Racket, static typing in the ML tradition, and a look at declarative and logic programming — asking of each what it makes easy, what it makes awkward, and why. Rather than treating a language as a fixed list of rules to memorize, we will build small interpreters for the languages we study, so that scope, binding, closures, evaluation order, higher-order functions, recursion, mutable state, and types stop being vocabulary and become things you have implemented yourself. Alongside that implementation work we will develop just enough formal semantics — grammars and operational rules — to say precisely what a program *means*, and to see why two languages that look alike on the page can behave very differently when you run them. The payoff is practical: by the end of the term you should be able to pick up an unfamiliar language quickly, read its documentation with the right expectations, and reason about the tradeoffs its designers were making.

## Course details

**Catalog description.** *Design concepts of high-level programming languages; survey of existing languages; experience using some languages.* 3 credits.

**Prerequisite.** CPT_S 223 or CPT_S 233 with a C or better.

**Texts.** There is no required textbook to purchase. We will read from these free sources, with specific sections listed in the schedule below:

- [*Programming Languages: Application and Interpretation*](https://www.plai.org/) (PLAI), Shriram Krishnamurthi — free online
- [*How to Design Programs*, 2nd ed.](https://htdp.org/) (HtDP), Felleisen, Findler, Flatt, and Krishnamurthi — free online
- [The Python 3 tutorial and language reference](https://docs.python.org/3/) and [The Racket Guide](https://docs.racket-lang.org/guide/)

**Software.** [Python 3.12+](https://www.python.org/downloads/) and [Racket 8.x](https://download.racket-lang.org/), both free and available for every platform. Everything we use in this course is free; there are no course or lab fees.

**Notes.** Lecture notes are posted here as we go, with runnable examples you can edit in the browser: [the λ-calculus](/notes/lambda/) <span class="tbd">(more to come)</span>.

**Expected effort.** This is a 3-credit course, so plan on roughly six to nine hours per week outside of lecture, most of it spent writing and debugging code.

## Learning outcomes

By the end of the term you should be able to:

1. Read and write idiomatic programs in an imperative/object-oriented language (Python 3) and a functional one (Racket), and explain the differences in how each structures a solution.
2. Describe a language's syntax with a grammar, and its behavior with operational rules, precisely enough that someone else could implement it.
3. Implement an interpreter for a small language, including environments, closures, recursion, and mutable state.
4. Explain and demonstrate the consequences of core design choices — scoping discipline, evaluation order, parameter passing, mutability, and static versus dynamic typing.
5. Learn an unfamiliar language quickly by recognizing which of these familiar mechanisms it is built from.

## Grading

| Weight | Component |
|---|---|
| 45% | Programming assignments (six, roughly biweekly) |
| 10% | In-class exercises and short quizzes |
| 20% | Midterm exam |
| 25% | Final exam |

{.compact}

Letter grades are assigned from the total weighted percentage on this scale:

| Percentage | Grade |
|---|---|
| 93 and above | A |
| 90–92.9 | A− |
| 87–89.9 | B+ |
| 83–86.9 | B |
| 80–82.9 | B− |
| 77–79.9 | C+ |
| 73–76.9 | C |
| 70–72.9 | C− |
| 67–69.9 | D+ |
| 60–66.9 | D |
| below 60 | F |

{.compact}

Totals are rounded to the nearest tenth of a percent, and no further rounding is applied. This scale is a floor: it may be adjusted in your favor at the end of the term, never against you.

## Course policies

**Assignments** are submitted through Canvas and are due at 11:59pm on the posted date. Each may be turned in up to 48 hours late at a penalty of 10% per day. You also have **three late days** for the term, no explanation needed — say in your submission comment that you are using one, and the penalty is waived for that day.

**Attendance** is not graded directly, but in-class exercises are collected the day they are given and cannot be made up outside of a university-approved absence. Come to class; this material is much easier to absorb in conversation than from a screenshot of someone else's notes.

**Exams** are taken in class on the scheduled day. Make-up exams are arranged in advance, for documented and university-approved absences. If something goes wrong the day of an exam, contact me as soon as you are able rather than after the fact.

**Regrades** may be requested within one week of a grade being posted, by email to me or a TA, saying specifically which part you believe was misgraded.

**Questions** are best asked in class or on the course discussion board, where the answer helps everyone. Email me or a TA for anything personal; we aim to reply within one business day.

## Academic integrity

Academic integrity is the cornerstone of the university. You are responsible for reading and understanding [WSU's Academic Integrity Policy](https://communitystandards.wsu.edu/policies-and-reporting/academic-integrity-policy/), which is grounded in Washington state law (WAC 504-26-010(3) and WAC 504-26-404).

***If you cheat on work in this class you will fail the course.*** You will also be reported to the Center for Community Standards. You have the right to appeal my decision; while an appeal is pending you may not drop or withdraw from the course. To ask for a change in my decision, use the form on the [Center for Community Standards](https://communitystandards.wsu.edu/) website.

In practice: talking through ideas, approaches, and error messages with your classmates is encouraged and is how most real programming gets done. What you submit must be written by you, and anything you took from a book, a website, or a classmate must be cited in a comment. Exams are closed-book. If you are ever unsure whether something is allowed, ask me before you turn it in — I would much rather answer that question than the other one.

## Use of AI tools

Large language models are good at exactly the thing this course asks you to learn to do yourself, which makes them easy to lean on in a way that quietly costs you the class. Unless an assignment says otherwise, you **may** use AI assistants to explain a concept, to walk through documentation, to interpret an error message, or to review code you have already written. You **may not** submit code or prose generated for you by a model, and you may not use any AI assistant during exams. If a model contributed to something you submit, say so in a comment saying what you used and how — an honest note costs you nothing, and an undisclosed one is handled under the academic integrity policy above.

## University policies and student resources

WSU maintains a [University Syllabus](https://syllabus.wsu.edu/university-syllabus/) covering the policies that apply to every course at the university: the Student Care Network, Lauren's Promise, reasonable accommodations for disability, pregnancy and related conditions, religious accommodation, and campus safety procedures. You are responsible for reading and understanding it.

If you need accommodations, reach out to the [Access Center](https://accesscenter.wsu.edu/) early in the term and then talk with me, so we can get things arranged well before an assignment or exam rather than during one.

## Schedule

Readings and topics are filled in as the term progresses; assignment due dates appear here as they are posted.

| Dates | Topics, readings, and due dates |
|---|---|
| **Aug 25, 27**<span class="wk">Week 1</span> | Syllabus and class policies; introduction to programming languages and paradigms; introduction to Python 3. |
| **Sep 1, 3**<span class="wk">Week 2</span> | TBD |
| **Sep 8, 10**<span class="wk">Week 3</span> | TBD |
| **Sep 15, 17**<span class="wk">Week 4</span> | TBD |
| **Sep 22, 24**<span class="wk">Week 5</span> | TBD |
| **Sep 29, Oct 1**<span class="wk">Week 6</span> | TBD |
| **Oct 6, 8**<span class="wk">Week 7</span> | TBD |
| **Oct 13, 15**<span class="wk">Week 8</span> | TBD |
| **Oct 20, 22**<span class="wk">Week 9</span> | TBD |
| **Oct 27, 29**<span class="wk">Week 10</span> | TBD |
| **Nov 3, 5**<span class="wk">Week 11</span> | TBD |
| **Nov 10, 12**<span class="wk">Week 12</span> | TBD |
| **Nov 17, 19**<span class="wk">Week 13</span> | TBD |
| **Nov 24, 26**<span class="wk">Week 14</span> | *Thanksgiving break, Nov 23–27 — no class, no office hours.* {.row-off} |
| **Dec 1, 3**<span class="wk">Week 15</span> | TBD |
| **Dec 8, 10**<span class="wk">Week 16</span> | TBD |
| **Dec 14–18**<span class="wk">Finals</span> | *There is no designated final exam.* |
