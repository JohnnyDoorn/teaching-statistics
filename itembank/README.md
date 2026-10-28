# Writing ANS questions as text files
Johnny van Doorn
2026-10-22

This folder shows how to write questions for **ANS**. Each question is a small text file. A builder turns a folder of these files into one package, and ANS imports that package with the parts that normally take hand work already in place, such as **model answers**, **grading criteria** for open questions, numeric fill-in gaps with a margin, multi-part exercises, data files and maths.

Why files instead of the ANS editor? Questions in files can be searched, reused, compared across years and checked. Every key in the examples below was recomputed from the data, and a coding agent can read and write the files too. Everything here was settled by test imports into ANS in September and October 2026. Where something is only partly tested, the agent instructions say so.

## Start here

New to all of this? This section takes you from nothing to your first package. If you only want to see the end result, skip to [Try it in ANS](#try-it-in-ans), which needs no setup at all.

<div>

> **Tip**
>
> One of the handy things about a language model is that you can ask it about itself. Stuck on an install step, an error message or a setting? Paste it in and ask what to do, or ask how a feature works. Before your agent runs, any chatbot (UvA AI Chat included) can help you get it running.

</div>

### What a coding agent is

You probably know chatbots like ChatGPT or UvA AI Chat. You type a question, it types an answer, and you copy out whatever it makes by hand. A **coding agent** is the same kind of language model, but it works on your own computer, inside a folder you give it. It opens and writes files there, runs programs such as R and Python, reads what comes back, and fixes its own mistakes before it reports to you. Because it works in your folder, you can also give it highly specific context, such as your lecture slides, lecture transcripts, literature, exam regulations and earlier exercises, and it reads them whenever it needs them.

For ANS questions, the most useful job is not writing new questions but cleaning up the ones you already have. A bank that has been reused for years collects wrong keys, model answers written for an earlier version of the numbers, page references to an older edition of the book, and links that no longer work. You give the agent an export of your bank, and it turns the questions into files, recomputes every key from the data, checks the model answers and book references, and lists what it would change. You decide, and it does the hand work. It can still be wrong about the statistics, so treat its output as a draft from a teaching assistant. The builder catches format mistakes, not wrong answers.

### Which agent to use

Several companies now offer a coding agent, and they all work in much the same way. The table lists the ones you are most likely to come across, with the cheapest plan that includes the agent. Prices change often; these were checked on 25 October 2026.

| Agent          | Company                        | Cheapest plan that includes it                                                                                      | Good to know                                                                                                                                                                                         |
|----------------|--------------------------------|---------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Claude Code    | Anthropic (US)                 | Claude Pro, \$20 a month ([pricing](https://claude.com/pricing))                                                    | The one I used for everything on this page                                                                                                                                                           |
| Codex          | OpenAI (US)                    | Every ChatGPT plan, the free one included; Plus is \$20 a month ([pricing](https://learn.chatgpt.com/docs/pricing)) | Many people already have a ChatGPT account                                                                                                                                                           |
| GitHub Copilot | GitHub, part of Microsoft (US) | Copilot Free, with monthly limits; Pro is \$10 a month ([plans](https://github.com/features/copilot/plans))         | Verified teachers can get Pro for free through [GitHub Education](https://docs.github.com/en/copilot/how-tos/copilot-on-github/set-up-copilot/enable-copilot/set-up-for-teachers-and-os-maintainers) |
| Mistral Vibe   | Mistral AI (France)            | Pro, \$14.99 a month; the free plan has a few coding sessions ([pricing](https://mistral.ai/pricing))               | A European company                                                                                                                                                                                   |

Any of them will do. The agent instructions are a plain text file and the tools are ordinary R and Python scripts, so every agent that can read files and run commands can follow them. I use Claude Code myself. If you pick it too, Claude Pro is enough to start (the free Claude plan does not include Claude Code), and the more expensive Max plans only matter if you hit the usage limits every day.

[UvA AI Chat](https://www.uva.nl/en/about-the-uva/about-the-university/ai/ai-in-education/uva-ai-chat/uva-ai-chat.html) is free for UvA staff and keeps your prompts within the UvA. For now it is a chat window, so it cannot work in your folder, but agentic use is expected soon. Once that arrives it becomes the natural choice, certainly for exam questions.

Whichever tool you use, never give it student data such as names, grades or answers. Also check whether you may share copyrighted or recorded material, such as a textbook PDF or a lecture transcript, before you put it in the agent’s folder. With a commercial account, also switch off the use of your chats for model training in its privacy settings.

### Set it up, once

1.  Install [VS Code](https://code.visualstudio.com), a free editor. Open its Extensions view (`Cmd+Shift+X` on a Mac, `Ctrl+Shift+X` on Windows), search for “Claude Code”, click *Install*, and sign in with your Claude account. (Would you rather not use an editor? The [Claude desktop app](https://claude.com/download) runs Claude Code too.) For another agent, install its extension instead, such as Codex or GitHub Copilot Chat. The steps below use Claude Code as the example; the other agents have the same parts under slightly different names.

2.  Install [R](https://cran.r-project.org) and [Python 3](https://www.python.org/downloads/) if you do not have them yet. On Windows, also install [Git for Windows](https://git-scm.com/downloads/win), which Claude Code uses to run commands and to download this folder.

3.  Make an empty folder for your questions, open it in VS Code (*File \> Open Folder*), and click *✻ Claude Code* in the bottom-right corner to open the agent.

4.  By default the agent decides for itself which steps are safe. Until you know how it behaves, click the mode indicator below the prompt box and choose *Manual*. It then shows every change and asks before most commands.

5.  Paste this as your first request.

    > Download only the itembank folder from https://github.com/JohnnyDoorn/teaching-statistics into this folder (the full repository is large, so do not clone all of it). Check that R with the packages exams and jsonlite, pandoc and Python 3 are installed, and install what is missing. Then build itembank/examples with itembank/tools/build-ans.R and tell me what you did.

6.  Give the agent its standing instructions. Your agent reads one file in your folder at the start of each conversation, `CLAUDE.md` for Claude Code and `AGENTS.md` for Codex, GitHub Copilot and many others, so you only have to say this once.

    > Create the instruction file you read at the start of every conversation (CLAUDE.md or AGENTS.md) in this folder. It should say to read itembank/ans-agent-instructions.qmd before reading, writing or building any ANS question, and that I review every question before it goes into ANS.

When it is done, open `itembank/examples/EXAMPLES.html` in your browser. If you see the twelve example questions with their answers, everything works.

### What I use it for

This is how I clean up a bank, for example the weekly assignments of a course. Each step is one request to the agent.

1.  Export your question bank from ANS (*Settings \> Export*, both the QTI 2.X export and the Respondus package) and put the zip files in your folder.
2.  “Convert the questions in these ANS exports into the format of itembank/examples, one file per question, in one folder per assignment. Change nothing in the content yet.”
3.  “Recompute every key from the data files, and list each question where the key, the model answer or the feedback disagrees with your result.”
4.  “Check every book reference in the model answers against the book in this folder, and list references to an older edition, to the wrong section, or to a page that does not say what the answer claims. Also list links that no longer work.” This works best with a text version of the book in your folder (see the caveat about copyrighted material above).
5.  “Go through the list with me, one question at a time.” You decide what changes, and the agent edits the files.
6.  “Build this folder with a TEST prefix.” Read the preview page it makes, which shows every question with its answer.
7.  Import the zip into a scratch bank, export it again, and ask “Compare this export with the expected file.” Then replace the old questions in your real bank.

A few habits help. Ask for one thing at a time and read each change before you accept it. The agent forgets everything between conversations except what is in that instruction file, so when you settle a convention (say, final answers in two decimals), ask it to add that to the file. And keep exam questions out of public places; this page lives in a public repository, which is why it holds only practice questions whose answers are already online.

## What is in this folder

| File or folder                                                                                                         | For whom          | What it holds                                                               |
|------------------------------------------------------------------------------------------------------------------------|-------------------|-----------------------------------------------------------------------------|
| `README.md` (this page)                                                                                                | you               | how it works, how to try it, how to write your own questions                |
| [`ans-agent-instructions.qmd`](https://johnnydoorn.github.io/teaching-statistics/itembank/ans-agent-instructions.html) | your coding agent | the question format, what ANS accepts and drops, and what is verified       |
| `examples/`                                                                                                            | both              | 12 example questions in 3 exercises, the built package and a preview        |
| `tools/`                                                                                                               | both              | the builder (R), the packaging step it calls (Python) and an import checker |

## The examples

Three exercises built on Smart Alex tasks from Chapter 9 (comparing two means) of *Discovering Statistics Using JASP* (Field, van Doorn & Wagenmakers, 2025). The scenarios and model answers follow the [Smart Alex answers on discoverjasp.com](https://discoverjasp.com/pages/smart_alex.html), and the questions link to the data files there rather than shipping copies. Every key was recomputed in R from those data files.

| Exercise                                             | Smart Alex | Questions | What it shows                                                                                                                       |
|------------------------------------------------------|------------|----------:|-------------------------------------------------------------------------------------------------------------------------------------|
| Spider anxiety (paired t-test)                       | 9.1-9.2    |         5 | a multi-part exercise with a shared scenario; single choice; tick-all; two numeric gaps in one question, accepting either sign of t |
| Pets and life satisfaction (t-test and linear model) | 9.5-9.6    |         5 | numeric gaps with a margin; a single fill-in gap with its explanation as feedback; tick-all with symbols                            |
| Backward messages (paired t-test)                    | 9.9        |         2 | single choice whose wrong options are named mistakes; an open question with grading criteria                                        |

[Preview every question with its answer](https://johnnydoorn.github.io/teaching-statistics/itembank/examples/EXAMPLES.html), or [download the package](https://johnnydoorn.github.io/teaching-statistics/itembank/examples/EXAMPLES.zip) and try it in ANS.

## Try it in ANS

1.  Download [`EXAMPLES.zip`](https://johnnydoorn.github.io/teaching-statistics/itembank/examples/EXAMPLES.zip).
2.  In ANS, open a question bank (a scratch bank is safest), go to **Settings \> Import** and choose **QTI 2.X package (Legacy)**. Not *QTI 3.0*, and not *Respondus*.
3.  Three exercises appear outside any folder, with titles that start with “EXAMPLE”. Move them to a folder if you like.
4.  Open them and look at what normally takes hand work. The choice and open questions have their model answers, the open question has its grading criteria, some questions hold two gaps (each with a margin), and the fill-in questions carry their explanation as feedback.

## Anatomy of a question file

A question file is plain text in the R/exams format, saved as `.qmd` (or `.Rmd`). Here is the part of the spider exercise with two gaps, `examples/1-spider-c-t.qmd`, slightly shortened:

``` markdown
Question
========
Test in JASP whether there is a difference between the mean anxiety for the picture and
the mean anxiety for the real spider. What is the calculated t-value? Round to two
decimal places.

t-value: ##ANSWER1##

Degrees of freedom: ##ANSWER2##

Answerlist
----------
*
*

Solution
========
The test statistic, t, is calculated by dividing the mean of differences by the standard
error of differences (t = -7/2.8311 = -2.47). ... When the same participants have been
used, the degrees of freedom are the sample size minus 1 (df = N - 1 = 11).

DSUJ §9.9, p. 384.

Meta-information
================
exname: dsj-09-spider-c-t
extype: cloze
exsolution: -2.47|11
exclozetype: num|num
extol: 0.01|0
expoints: 0.5
exextra[alt]: 2.47|
exextra[exercise]: dsj-09-spider
exextra[anstitle]: discoverjasp Smart Alex 9.1-9.2 - spider anxiety (paired t-test)
```

**Question** is what students see. Its **Answerlist** holds the options of a choice question, or one empty line per gap. **Solution** becomes the model answer, and in a choice question a second Answerlist marks the options, with one `* True.` or `* False.` line per option in the same order. **Meta-information** says what kind of question it is and what the key is.

| Field                 | What it does                                                                                                                                                 |
|-----------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `extype`              | `schoice` (single choice), `mchoice` (tick-all), `num` (one numeric gap), `cloze` (several gaps) or `string` (an open question, with the essay fields below) |
| `exsolution`          | the key, as `100` for options (1 marks a correct one), a number for a gap, or `-2.47\|11` for several gaps                                                   |
| `extol`               | the margin of each numeric gap                                                                                                                               |
| `expoints`            | the points; in a `cloze`, the points per gap                                                                                                                 |
| `exshuffle`           | shuffles the options once, when the package is built (ANS can also shuffle per student)                                                                      |
| `exextra[exercise]`   | questions with the same value become one exercise with several parts                                                                                         |
| `exextra[anstitle]`   | the title ANS shows; parts of one exercise share it                                                                                                          |
| `exextra[alt]`        | extra accepted answers per gap, with `\|` between the gaps                                                                                                   |
| `exstringtype: essay` | with `exextra[essay,logical]: TRUE`, turns a `string` question into an open question with an essay field                                                     |

In an open question, a **Criteria** list at the end of the solution becomes the grading criteria in ANS (see `examples/3-darklord-b-essay.qmd`). The [R/exams website](https://www.r-exams.org) documents the format in full, including questions that draw new numbers every time they are built.

## What the builder checks for you

The build stops, with a message that says what to fix, when

1.  a tick-all question does not say that at least one statement is true (ANS’s correction for guessing assumes it);
2.  a gap has no label right before it (students miss unlabelled gaps);
3.  a question holds more than one answer field, apart from several numeric gaps;
4.  the grading criteria of an open question do not add up to its points;
5.  the parts of an exercise do not sort next to each other.

## What ANS cannot import

- **A model answer for a fill-in question.** The builder puts the explanation in as question feedback instead, the same text for a right and a wrong answer.
- **A separate text block above a multi-part exercise.** The shared scenario ends up at the top of the first question, which reads fine.
- **Folders and tags.** Imported questions land outside any folder.

## My conventions

These are my choices, not rules of ANS, so adapt them to your course.

- The correct option comes first in the file; the build shuffles.
- Numeric answers are fill-in gaps with a margin rather than ANS’s own numerical question type, because a gap shows you exactly what students typed. In a question with several gaps, the builder also accepts the obvious variants of each key, such as `0,035` and `.035` for `0.035`.
- Every wrong option is a mistake you can name, and the model answer names it.
- Every model answer ends with its source (book section and page, lecture slides), for whoever checks the questions.
- Keys are computed from the data, never copied from a published answer.
- The question states alpha whenever the answer depends on it.

## Check an import

After an import, export the bank again (**Settings \> Export**, the QTI 2.X export and, if you like, the Respondus package), unzip it, and run

``` sh
python3 tools/check-import.py path/to/export --expected examples/EXAMPLES.expected.json
```

It prints one line per check (titles, parts, points, model answers, feedback, accepted answers, criteria, attachments) and marks failures with `XX`. The check that matters most is that no model answer ended up in the text students see.

## Credits

- Field, A., van Doorn, J., & Wagenmakers, E.-J. (2025). *Discovering statistics using JASP*. Sage. Smart Alex tasks, answers and data at [discoverjasp.com](https://discoverjasp.com).
- Zeileis, A., Umlauf, N., & Leisch, F. (2014). Flexible generation of e-learning exams in R: Moodle quizzes, OLAT assessments, and beyond. *Journal of Statistical Software, 58*(1), 1-36. <https://doi.org/10.18637/jss.v058.i01>
- The questions link to the [JASP video library](https://jasp-stats.github.io/jasp-video-library/) for how to run each analysis.
- The tools are free software under this repository’s GPL-3 license.
