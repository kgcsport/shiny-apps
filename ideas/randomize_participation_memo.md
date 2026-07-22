# Memo: Classroom Participation as Structured Roles

I am developing a classroom participation system for an introductory economics course and would appreciate feedback on both the pedagogy and possible research design.

## Motivation

Class participation is usually treated as a single behavior: students either “participate” or they do not. In practice, participation includes many different tasks. Some students are comfortable asking questions but not answering them. Others can prepare a strong summary but dislike speaking spontaneously. Some can explain graphs well but avoid cold calls. Standard participation grading often misses these distinctions and may reward students who are already comfortable speaking in class.

The goal of this project is to make participation more structured, measurable, and varied. I want students to have multiple ways to contribute while also creating data that help us understand how students learn to participate.

## Basic system

Students earn participation tokens by completing class jobs and making useful live contributions. Tokens serve two purposes.

First, gross lifetime tokens earned determine the participation component of the course grade. Second, students have a spendable token balance that can be used on course goods, such as problem set extensions, class public goods, or end-of-semester grade reweighting. Spending tokens does not reduce the participation credit they have already earned.

The system separates participation into job categories such as:

* summary of last class
* summary of prep materials
* real-world example
* question writer
* graph explainer
* mistake detective
* cold-call eligible
* voluntary live answer or question

Each job has a posted token wage. If a student completes the job satisfactorily, they receive the full wage. If they make a serious but weak attempt, they receive half the wage. If they make no meaningful attempt, they receive zero.

## Random assignment and later choice

In the first one or two weeks, students are randomly assigned to participation jobs. This gives students experience with different forms of participation and creates random variation in early exposure to specific roles.

Later in the semester, students can express willingness to do different job categories. The current plan is to use weekly rounds. Students report the minimum token wage they would require to do each job category. The system can then set wages using a simple market-clearing rule: for each job category, select the cheapest students needed to fill the available slots and set a common wage equal to the highest accepted bid. I may also test an application-ticket version where students allocate weekly tickets across job pools and assignments are drawn randomly, weighted by tickets.

The main idea is that students first experience participation roles through random assignment and later reveal which roles they are willing to take, at what token wage, and how well they perform.

## Research question

The central research question is:

**Does early random exposure to a participation role change later willingness to participate in that role, stated minimum token wages, and actual performance?**

This turns the course into a small test of how students learn to participate. The paper would focus primarily on education and classroom participation, not on the token economy itself.

Possible outcomes include:

* later willingness to select the same job category
* stated minimum token wage for that category
* application tickets allocated to that category
* actual assignment and completion
* full/half/zero wage performance
* voluntary live participation
* changes in comfort or confidence

A simple empirical specification would compare student-by-job-category outcomes after the random assignment period:

`Outcome_ij = student fixed effect + job-category fixed effect + β(Randomly assigned to job j early)_ij + error_ij`

where `i` indexes students and `j` indexes job categories.

The coefficient β would show whether early exposure to a specific participation role changes later willingness or performance in that role.

## Baseline survey

A baseline survey seems critical. Before random assignment, I would ask students about their comfort with different forms of participation and their initial willingness to do each job.

Possible baseline measures include:

* comfort speaking in class
* concern about being wrong publicly
* preference for written versus spoken participation
* comfort explaining graphs
* comfort summarizing readings or prep materials
* comfort asking questions
* comfort being called on
* prior participation habits
* confidence in economics and quantitative material

I would also ask students to report the minimum number of participation tokens they would need to willingly complete each job category. These baseline willingness-to-accept measures would be used as pre-treatment measures, not necessarily to set the initial wages. My current plan is to start with a simple tiered wage schedule during the random-assignment period, then use later student bids to adjust wages.

## Economics framing

Although the paper would be primarily about classroom participation, there is a useful economics framing. Different participation roles have different nonpecuniary costs: preparation time, public visibility, spontaneity, difficulty, and risk of being wrong. Token wages provide a way to measure the compensation students require to accept those roles.

In that sense, the minimum token wage is a classroom analogue of a compensating differential. But I see this as a supporting framework rather than the main contribution. The main contribution is to decompose participation into role-specific tasks and study whether experience changes students’ willingness to participate.

## Questions where I would value feedback

1. Is the main research question useful and clear?
2. Does the random early job assignment create a credible design for studying later participation choices?
3. What baseline survey measures are most important?
4. Should initial wages be simple and instructor-set, or informed by baseline willingness-to-accept responses?
5. Are there participation roles that should be added, removed, or renamed?
6. What outcomes would be most convincing to education researchers?
7. Does the token system create any obvious fairness, incentive, or interpretation problems?
8. Is the compensating-differentials framing helpful, or distracting?
9. What literature on classroom participation, cold calling, active learning, or participation grading should I be reading?
10. What would make this publishable as an economics education paper rather than just a course-management tool?

## Current view

My current view is that the strongest version of the paper is not “I built a participation app.” Instead, it is:

**Class participation is multidimensional. Random early exposure to structured participation roles can reveal whether students learn to participate, which roles they later choose, and how costly different forms of participation feel to them.**

The software is the infrastructure that makes this design feasible, but the paper should focus on participation behavior, role-specific costs, and how students’ willingness to participate changes with experience.
