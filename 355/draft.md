---
title: CptS 355 — Programming Language Design
subtitle: Working draft · Washington State University · Fall 2026
description: Working draft of the CptS 355 syllabus and schedule. Not the published version.
keywords: cpts 355, programming language design, wsu, washington state university, interpreters, semantics, python, racket, functional programming, thomas gilray
bg: fewcritters
robots: noindex, nofollow, noarchive
---

<div class="note" markdown="1">
**This is the planning draft, not the syllabus.** Weeks are penciled in here first; nothing on this page is committed until it is copied into [the published page](/355/). If you are taking the course, read that one instead.
</div>

<div class="page-people" markdown="1">

**Instructor:** Thomas Gilray {{email:thomas.gilray@wsu.edu}} — EME B49, office hours <span class="tbd">TBD</span>

**Teaching assistants:** Aidan Johnson {{email:aidan.johnson@wsu.edu}} and Joseph Buchholz {{email:joseph.buchholz@wsu.edu}} — office hours <span class="tbd">TBD</span>

**Lectures:** Tuesdays and Thursdays, 4:20–5:35pm, in [SEH 105](https://maps.google.com/?q=46.7288,-117.1682)

</div>

Programming Language Design is a tour of the surprisingly small set of ideas that nearly every programming language is assembled from. We will work in Python, Racket, Haskell, Rust, and C++, asking of each what it is built to make easy, what it gives up in exchange, and when that trade is the one you want. Rather than memorizing a language as a list of rules, we will build small interpreters of our own, so that scope, binding, closures, evaluation order, recursion, mutable state, and types become things you have studied closely or even implemented, rather than terms you have read. Alongside that comes just enough formal semantics (grammars and operational rules) to say precisely what a program *means*, and to see why two languages that look alike on the page can behave very differently when you run them. By the end you should be able to pick up an unfamiliar language quickly and reason about the tradeoffs its designers were making.

## Course details

**Catalog description.** *Design concepts of high-level programming languages; survey of existing languages; experience using some languages.* 3 credits.

**Prerequisite.** CPT_S 223 or CPT_S 233 with a C or better.

**Resources.** There is no textbook for this course. Readings will come from freely available and custom materials, listed in the weekly schedule below. These are all free, and any of them may help:

- [*Programming Languages: Application and Interpretation*](https://www.plai.org/) (PLAI), Shriram Krishnamurthi — interpreters and semantics, the closest to what we do here
- [*How to Design Programs*, 2nd ed.](https://htdp.org/) (HtDP), Felleisen, Findler, Flatt, and Krishnamurthi
- [*Learn You a Haskell for Great Good!*](https://learnyouahaskell.github.io/) — a gentle way into typed functional programming
- [*Write You a Haskell*](https://github.com/sdiehl/write-you-a-haskell), Stephen Diehl — building a typed functional language from nothing
- [*The Rust Programming Language*](https://doc.rust-lang.org/book/) and [*Rust by Example*](https://doc.rust-lang.org/rust-by-example/) — the standard introductions
- [*Learning Rust With Entirely Too Many Linked Lists*](https://rust-unofficial.github.io/too-many-lists/) — the clearest explanation of ownership and borrowing anywhere
- [*Crafting Interpreters*](https://craftinginterpreters.com/), Robert Nystrom — builds two complete interpreters for the same language, one simple and one fast
- [The Python 3 tutorial and language reference](https://docs.python.org/3/) and [The Racket Guide](https://docs.racket-lang.org/guide/)

**Recommended.** These are not free and none is required. Each is excellent, and worth owning if you want to go further than this course can take you:

- [*Essentials of Programming Languages*, 3rd ed.](https://eopl3.com/) (EOPL), Friedman and Wand (MIT Press, 2008, ISBN 978-0-262-06279-4) — the classic interpreter-first course text, and the deepest treatment of environments, closures, and continuations
- [*Lisp in Small Pieces*](https://christian.queinnec.org/WWW/LiSP.html), Christian Queinnec (Cambridge, 2003, ISBN 978-0-521-54566-2) — eleven interpreters and two compilers for Scheme, each more sophisticated than the last
- [*Types and Programming Languages*](https://www.cis.upenn.edu/~bcpierce/tapl/) (TAPL), Benjamin Pierce (MIT Press, 2002, ISBN 978-0-262-16209-8) — the standard reference on type systems, if the typing material grabs you
- [*A Tour of C++*, 3rd ed.](https://www.stroustrup.com/tour3.html), Bjarne Stroustrup (Addison-Wesley, 2022, ISBN 0-13-681648-7) — covers C++20 in about 300 pages, the quickest route into modern C++

**Software.** Everything we use is free and runs on every platform: [Python 3.12+](https://www.python.org/downloads/), [Racket 8.x](https://download.racket-lang.org/), [Rust](https://rustup.rs/) (installed with rustup), [GHC](https://www.haskell.org/ghcup/) for Haskell (installed with GHCup), and [clang](https://clang.llvm.org/get_started.html) for C++. There are no course or lab fees.

**Expected effort.** This is a 3-credit course, so plan on roughly six to nine hours per week outside of lecture, most of it spent writing and debugging code.

## Learning outcomes

By the end of the term you should be able to:

1. Read and write idiomatic programs in an imperative/object-oriented language (for example, Python 3, C++, or Rust) and a functional one (for example, Racket or Haskell), and explain the differences in how each structures a solution.
2. Describe a language's syntax with a grammar, and its behavior with operational rules, precisely enough that someone else could implement it.
3. Implement an interpreter for a small language, including environments, closures, recursion, and mutable state.
4. Explain and demonstrate the consequences of core design choices — scoping discipline, evaluation order, parameter passing, mutability, and static versus dynamic typing.
5. Learn an unfamiliar language quickly by recognizing which of these familiar mechanisms it is built from.

## Grading

| Weight | Component |
|---|---|
| 50% | Five written exams, in class at designated times — 10% each |
| 30% | Three coding exams, proctored — 10% each |
| 20% | Study group: weekly meeting reports and a final group project |
| +10% | Coding exercises, released throughout the term |

{.compact}

The **five written exams** are taken in class at times posted on the schedule below. The **three coding exams** are proctored; scheduling details are forthcoming. Every exam, written or coding, is closed book, closed internet, and closed model, with one exception: you may bring a single page of notes, front and back, printed or handwritten. You are welcome to build those notes collaboratively within your study group, but each student must bring their own page. You may rewrite your page before each exam, and probably should as the material moves on.

The **coding exercises** are additive, so there are more points available in this course than a perfect grade requires. Treat them as bonus credit: completing them can make up for points lost elsewhere.

### Study groups

You will spend the term working in a study group. **By Friday, August 28**, one member of each proposed group should email me the names of everyone in it, using the subject line **CptS 355 Group** and including the word **open** or **closed** to say whether I may add another student. Please do not register yourself as a closed group of one unless you are genuinely certain you want to work alone this term; if you are, include your unofficial transcript and a short note on why working alone is the right call for you. Anyone without a group by that Friday will be placed in one.

Each group agrees on a fixed weekly time and keeps it for the rest of the term, meeting either in person or on Zoom. Each week one member rotates into the role of **scribe** and writes up a half page of thoughtful bulleted notes: who attended, what the group discussed, and what you learned. Those weekly reports are **10%** of your grade.

The remaining **10%** is a **final project** carried out in your study group. You will have an opportunity to select a project related to your interests, with some flexibility in scope; a proposal deadline and further details will come with the assignment.

### Letter grades

Cut-offs can move down but never up: an A or A− will not require more than 90%, a B or B− more than 80%, a C or C− more than 70%, and a D more than 60%. Totals are rounded to the nearest tenth of a percent. A substantial curve on grade cut-offs is unlikely.

## Course policies

**Coding exercises** are released throughout the term and submitted online. No individual exercise has its own deadline: any of them may be submitted for credit at any point through the last day of lecture, Thursday, December 10. They track exactly what the exams cover, though, so the release schedule is still the pace you want to keep.

**Attendance** is not graded directly, but the five written exams are taken in class on the day they are scheduled, so those days are not optional. Come to the others too; this material is much easier to absorb in conversation than from a screenshot of someone else's notes.

**Exams** — written exams are taken in class on the scheduled day; coding exams are proctored at times announced during the term. Make-up exams are arranged in advance, for documented and university-approved absences. If something goes wrong the day of an exam, contact me as soon as you are able rather than after the fact.

**Study group reports** are due each week and submitted by that week's scribe.

**Regrades** may be requested within one week of a grade being posted, by email to me or a TA, saying specifically which part you believe was misgraded.

**Questions** are best asked in class or on the course discussion board, where the answer helps everyone. Email me or a TA for anything personal; we aim to reply within one business day.

## Academic integrity

Academic integrity is a cornerstone of the university. You are responsible for reading and understanding [WSU's Academic Integrity Policy](https://communitystandards.wsu.edu/policies-and-reporting/academic-integrity-policy/), which is grounded in Washington state law (WAC 504-26-010(3) and WAC 504-26-404).

In this course, cheating is almost entirely about one thing: **giving or getting unauthorized assistance on a proctored exam**, whether one of the five written exams or one of the three coding exams. That is where your grade is earned, and it is the one place where collaboration is off limits. Everything else is open. Studying together, working through the extra-credit coding exercises with whoever you like, and using whatever tools help you are all permitted without restriction; see [Use of AI tools](#ai) below. The only other way to run into trouble is misrepresenting your group's work, such as reporting attendance at a study group meeting that did not happen; that is treated the same way. If you are ever unsure whether something crosses either line, ask me first — I would much rather answer that question than the other one.

***If you cheat on an exam in this class you will fail the course.*** You will also be reported to the Center for Community Standards. You have the right to appeal my decision; while an appeal is pending you may not drop or withdraw from the course. To ask for a change in my decision, use the form on the [Center for Community Standards](https://communitystandards.wsu.edu/) website.

## Use of AI tools {: #ai }

Primary assessment in this course is proctored: five written exams in class and three proctored coding exams. Apart from the one page of notes you bring, those are closed book, closed internet, and closed model, and they are where your grade actually comes from. Because of that, I have no reason to police how you prepare for them.

So outside of the exams, you are encouraged to make delimited, thoughtful use of AI. You are permitted to treat an AI as you would a human classmate and use one arbitrarily on any out-of-class work — the extra-credit coding exercises, your study group meetings, and your group's final project all included. Ask it to explain a concept, walk you through documentation, review code you wrote, or argue with you about a design choice. Collaboration with your classmates on the same work is equally unrestricted.

How you use that freedom is your call, and the discipline you bring to it will matter more than any rule I could write here. A study partner who does the work for you leaves you with nothing to show in a proctored room. Use a model to reach understanding faster and to get yourself unstuck, not to alienate yourself from your own process of learning.

## University policies and student resources

WSU maintains a [University Syllabus](https://syllabus.wsu.edu/university-syllabus/) covering the policies that apply to every course at the university: the Student Care Network, Lauren's Promise, reasonable accommodations for disability, pregnancy and related conditions, religious accommodation, and campus safety procedures. You are responsible for reading and understanding it.

If you need accommodations, reach out to the [Access Center](https://accesscenter.wsu.edu/) early in the term and then talk with me **at least two weeks before any exam**, so that everything is arranged in advance rather than during one.

## Schedule

Topics and readings will be posted here ahead of time. Check back once or twice a week to stay up to date.

| Dates | Topics, readings, and due dates |
|---|---|
| **Aug 25, 27**<span class="wk">Week 1</span> | Syllabus and class policies; introduction to programming languages and paradigms; recursive vs imperative control; introduction to Python 3. |
| **Sep 1, 3**<span class="wk">Week 2</span> | Syntax and semantics; recursive-descent parsing; interpreters. |
| **Sep 8, 10**<span class="wk">Week 3</span> | C/C++; structured and object-oriented programming; memory management; TBD. |
| **Sep 15, 17**<span class="wk">Week 4</span> | **Exam 1 is on Tuesday, Sept 15.** Topics TBD. |
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
