---
title: Hello, world!
headerImg: sea.jpg
date: 2024-01-05
---


## A Programming Language

### Two Variables

- `x`, `y`

### Three Operations

- `x++`
- `x--`
- `x = 0 ? L1 : L2`


<br>
<br>
<br>
<br>
<br>
<br>


## Example Program

(What _does_ it do?)

```
L1: x++
    y--
    y = 0 ? L2 : L1
L2: ...
```


<br>
<br>
<br>
<br>
<br>
<br>

## The above language is "equivalent to" every PL!

But good luck writing

- ... QuickSort

- ... Fortnite

- ... Spotify!

<br>
<br>
<br>
<br>
<br>
<br>

## So Why Study Programming Languages?

![Federico Fellini](/static/img/fellini.png){#fig:fellini .align-center width=25%}

> A different language
> is
> a different vision
> of life.


<br>
<br>
<br>
<br>
<br>
<br>


## So Why Study Programming Languages?

> The principle of **linguistic relativity**
> holds that the structure of a language
> affects its speakers world view or cognition.

Or more simply:

> Programming Language
> shapes
> Programming Thought.

Language affects how ideas and computation are expressed


<br>
<br>
<br>
<br>
<br>
<br>



## Course Goals

![130 Brain](/static/img/galaxy-brain-130.jpg){#fig:galaxy .align-center width=65%}


<br>
<br>
<br>
<br>
<br>
<br>



### New languages come (and go ...)

There was no

- Java        30 years ago
- C#          25 years ago
- Rust        15 years ago
- WebAssembly  5 years ago

<br>
<br>
<br>
<br>
<br>
<br>

## What is CSE 130 about?

- Concepts in programming languages
- Programming paradigms
- Language design and implementation

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Goal: Learn the Anatomy of PL

![Anatomy](/static/img/anatomy.png){#fig:anatomy .align-center width=20%}

- What makes a programming language?
- Which features are **fundamental** and which are **syntactic sugar**?


<br>
<br>
<br>
<br>
<br>
<br>


## Goal: Learn New Languages / Constructs

![Musical Score](/static/img/music-score.png){#fig:music .align-center width=30%}

New ways to **describe** and **organize** computation,
to create programs that are:

- **Correct**
- **Readable**
- **Extendable**
- **Reusable**



<br>
<br>
<br>
<br>
<br>
<br>






## Goal: How to Design new Languages

New languages being designed in industry as we speak:

- Flow, React @ Facebook
- Rust @ Mozilla
- TypeScript @ Microsoft
- Swift @ Apple
- WebAssembly @ Google + Mozilla + Microsoft

Buried in every large system is a (domain-specific) language

- DB: SQL
- Word, Excel: Formulas, Macros, VBScript
- Emacs: LISP
- Latex, shell scripts, makefiles, ...

If you work on a large system, you **will** design a new PL!

<br>
<br>
<br>
<br>
<br>
<br>

## Goal: Enable You To Choose Right PL

But isn't that decided by

- Libraries
- Standards
- Hiring
- Your Boss?!

Yes.

**My goal:** Educate tomorrow's leaders so you'll make **informed** choices.



<br>
<br>
<br>
<br>
<br>
<br>

## What is CSE 130 **not** about?

Learning...

- JavaScript in January
- Haskell in February
- C++ in March

etc.

<br>
<br>
<br>
<br>
<br>
<br>

## The Crew

### Profs

[Ranjit Jhala](https://ranjitjhala.github.io/)

- Professor at CSE since 2005

- Research: Tools and Techniques to make programs better

**Many thanks to for taking over in weeks 1-2!**

[Prof. Nadia Polikarpova](https://cseweb.ucsd.edu/~npolikarpova)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## The Crew

### Teaching Assistants

- [Matt Kolosick](mailto:mkolosick@ucsd.edu)
- [Cole Kurashige](mailto:ckurashige@ucsd.edu)
- [George Sakkas](mailto:gsakkas@ucsd.edu)

### Tutors

- [Rana Lulla](mailto:rlulla@ucsd.edu)
- [Matthew Peng](mailto:mapeng@ucsd.edu)
- [Melody Ruth](mailto:maruth@ucsd.edu)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Course Syllabus

**Functional Programming**

- Lambda calculus (2 weeks)
- Haskell (6 weeks)

**Logic Programming**

- Prolog (2 weeks, maybe)

<br>
<br>
<br>
<br>
<br>
<br>

## **QuickSort** in C

```c
void sort(int arr[], int beg, int end){
  if (end > beg + 1){
    int piv = arr[beg];
    int l = beg + 1;
    int r = end;
    while (l != r-1)
       if(arr[l] <= piv) l++;
       else swap(&arr[l], &arr[r--]);
    if(arr[l]<=piv && arr[r]<=piv)
       l=r+1;
    else if(arr[l]<=piv && arr[r]>piv)
       {l++; r--;}
    else if (arr[l]>piv && arr[r]<=piv)
       swap(&arr[l++], &arr[r--]);
    else r=l-1;
    swap(&arr[r--], &arr[beg]);
    sort(arr, beg, r);
    sort(arr, l, end);
  }
}
```



<br>
<br>
<br>
<br>
<br>
<br>

## **QuickSort** in Haskell

```Haskell
sort []     = []
sort (x:xs) = sort ls ++ [x] ++ sort rs
  where
    ls      = [ l | l <- xs, l <= x ]
    rs      = [ r | r <- xs, x <  r ]
```

(**Note:** not a wholly [fair comparison...](http://stackoverflow.com/questions/7717691/why-is-the-minimalist-example-haskell-quicksort-not-a-true-quicksort))


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Course Logistics

[webpage](https://ucsd-cse130.github.io/wi24)

- Calendar
- Lecture notes
- Programming assignments

[piazza](https://piazza.com/class/lr1zj9yl963y9)

- Go-to place if you have a question or need help

<br>
<br>
<br>
<br>
<br>
<br>

## Grading

- 5%  Class participation (clicker questions)
- 25% Programming Assignments
- 35% Two Midterm **Tu 1/30** and **Thu 2/22**
- 35% Final       **Tu 3/19**
- 05% Piazza Extra Credit
  - To **top 20** best participants

<br>
<br>
<br>
<br>
<br>
<br>

## Assignments

**6 programming assignments**

- Released [online](assignments.html)
- At least a week before due date
- Via github classroom

**Six late days**

- used as **whole unit**
- 5 mins late = 1 late day
- at most 4 per assignment

**No Groups**

- Assignments must be submitted individually via github
- Ok to discuss with classmates, but solution must be your own

<br>
<br>
<br>
<br>
<br>
<br>

## Exams

- Must be done at allotted time and location

- 2-sided ``cheat sheet''

- The final is cumulative


<br>
<br>
<br>
<br>
<br>
<br>


## Clickers

<!--
Assigned Seating

- From next lecture
- See your [group](/static/raw/seating.txt) and [seat](https://ucsd-cse130.github.io/wi20/static/raw/Center_113_groups.pdf
-->

Make class interactive

- Help **you** and **me** understand what's tricky

Not Optional

- **Cheap** ones are fine
- **Respond** to 75% questions


<br>
<br>
<br>
<br>
<br>
<br>

## Clicker Protocol

1. **Solo Vote**
    - Think for yourself, select answer

2. **Discuss**
    - Analyze problem in groups
    - Reach consensus
    - Have questions, raise your hand!

3. **Group Vote**
    - Everyone in group votes
    - Hopefully the same way but not enforced
    - You *don't have to answer correctly* to get points!

4. **Class Discuss**
    - What was easy or tricky?

<br>
<br>
<br>
<br>
<br>
<b>


## Your Resources

### Discussion section

- Friday 3:00-3:50pm (PCYNH 109)

### Office hours

- Every day, check **CANVAS calendar**

### Piazza

- We answer during work hours and office hours (M-F)

### No text

- Online lecture notes and links

<br>
<br>
<br>
<br>
<br>
<br>






## Academic Integrity

Programming assignments: do not copy from classmates or from previous years

Exams done **alone**

- Zero Tolerance
- Offenders punished ruthlessly
- Please see academic integrity statement


<br>
<br>
<br>
<br>
<br>
<br>


## Students with Disabilites

Students requesting accommodations for this course due to
a disability or current functional limitation must provide
a current **Authorization for Accommodation (AFA)** letter
issued by the Office for Students with Disabilities (OSD)
which is located in University Center 202 behind Center Hall.

Students are required to present their AFA letters to Faculty
(please make arrangements to contact me privately) and to the
CSE OSD Liaison [Christina Rontell](https://cse.ucsd.edu/people/administrative-staff/christina-rontell)
in advance so that accommodations may be arranged.

<br>
<br>
<br>
<br>
<br>
<br>

## Diversity and Inclusion

### Goal

- Create a diverse and inclusive learning environment
- Where all students feel comfortable and can thrive.
- *Please let us know* if there is a way to make you feel more included
- In person, via email/discussion boar

### Expectations

- Students will honor and respect your classmates!
- Abide by the UCSD [Principles of Community](https://ucsd.edu/about/principles.html)
- Understand that others’ backgrounds, perspectives and experiences will be different
- Help build an environment where everyone is respected and comfortable.

<br>
<br>
<br>
<br>
<br>
<br>
