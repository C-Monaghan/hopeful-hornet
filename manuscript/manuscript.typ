// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}

// ============================================================================
// CUSTOM TYPST TEMPLATE
// Author: Cormac Monaghan
// Description: General-purpose Typst template for academic manuscripts
// Supports: multiple authors, optional ORCID, affiliations, correspondence, etc.
// ============================================================================

// Credit to Christopher Kenny's ctk-article
// https://github.com/christopherkenny/ctk-article/blob/main/_extensions/ctk-article/typst-template.typ
// better way to avoid escape characters, rather than doing a regex for \\@
#let to-string(content) = {
  if content.has("text") {
    content.text
  } else if content.has("children") {
    content.children.map(to-string).join("")
  } else if content.has("body") {
    to-string(content.body)
  } else if content == [ ] {
    " "
  }
}

// Instead of linking to an external ORCID image, we inline the SVG directly.
#let orcid_svg = str(
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
  <!-- Generator: Adobe Illustrator 19.1.0, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
  <svg version=\"1.1\" id=\"Layer_1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" x=\"0px\" y=\"0px\"
    viewBox=\"0 0 256 256\" style=\"enable-background:new 0 0 256 256;\" xml:space=\"preserve\">
  <style type=\"text/css\">
    .st0{fill:#A6CE39;}
    .st1{fill:#FFFFFF;}
  </style>
  <path class=\"st0\" d=\"M256,128c0,70.7-57.3,128-128,128C57.3,256,0,198.7,0,128C0,57.3,57.3,0,128,0C198.7,0,256,57.3,256,128z\"/>
  <g>
    <path class=\"st1\" d=\"M86.3,186.2H70.9V79.1h15.4v48.4V186.2z\"/>
    <path class=\"st1\" d=\"M108.9,79.1h41.6c39.6,0,57,28.3,57,53.6c0,27.5-21.5,53.6-56.8,53.6h-41.8V79.1z M124.3,172.4h24.5
      c34.9,0,42.9-26.5,42.9-39.7c0-21.5-13.7-39.7-43.7-39.7h-23.7V172.4z\"/>
    <path class=\"st1\" d=\"M88.7,56.8c0,5.5-4.5,10.1-10.1,10.1c-5.6,0-10.1-4.6-10.1-10.1c0-5.6,4.5-10.1,10.1-10.1
      C84.2,46.7,88.7,51.3,88.7,56.8z\"/>
  </g>
  </svg>"
)

// -----------------------------------------------------------------------------
// Core Function: `shifu-article(...)`
// -----------------------------------------------------------------------------
#let shifu-article(
    // --- Metadata and layout parameters ---
    title: none,
    subtitle: none,
    authors: none,
    date: none,
    abstract: none,
    abstract-title: none,
    keywords: none,
    correspondence: none,
    published: none,
    code: none,
    cols: 1,
    margin: (x: 1in, y: 1in),
    paper: "us-letter",
    lang: "en",
    region: none,
    font: (),
    fontsize: 11pt,
    mathfont: "New Computer Modern Math",
    codefont: "DejaVu Sans Mono",
    sectionnumbering: none,
    toc: false,
    block-author: none,
    toc_title: none,
    toc_depth: none,
    toc_indent: 1.5em,
    linestretch: 1,
    linkcolor: "#800000",
    title-page: false,
    author-note: none,
    doc,
    ) = {
  // ---------------------------------------------------------------------------
  // PAGE AND TYPOGRAPHY SETTINGS
  // ---------------------------------------------------------------------------
  // Set up margins, paper size, and page numbering scheme.
  set page(
    paper: paper,
    margin: margin,
    numbering: "1",
  )
  // Paragraph defaults — fully justified text with standard indent.
  set par(
    justify: true,
    first-line-indent: 1em
    )

  // Define global text style: language, font, size, etc.
  set text(
    lang: lang,
    region: region,
    font: font,
    size: fontsize)

  // Special fonts for math and code blocks
  show math.equation: set text(font: mathfont)
  show raw: set text(font: codefont)

  // ---------------------------------------------------------------------------
  // FIGURE CAPTION FORMAT
  // ---------------------------------------------------------------------------
  // Defines how figure/table captions should be displayed.
  // Adds bold numbering and aligns captions to the left margin.
  show figure.caption: it => [
    #v(-1em)
    #align(left)[
      #block(inset: 1em)[
        #text(weight: "bold")[
          #it.supplement
          #context it.counter.display(it.numbering)#it.separator
        ]
        #it.body
      ]
    ]
  ]

  set heading(numbering: sectionnumbering)

  // ---------------------------------------------------------------------------
  // LINK AND REFERENCE COLORS
  // ---------------------------------------------------------------------------
  show link: this => {
    if type(this.dest) != label {
        text(this, fill: rgb(linkcolor.replace("\\#", "#")))
    } else {
        text(this, fill: rgb("#0000CC"))
    }
  }

  show ref: this => {
    text(this, fill: rgb("#640872"))
  }

  show cite.where(form: "prose"): this => {
    text(this, fill: rgb("#640872"))
  }

  // ---------------------------------------------------------------------------
  // FRONT MATTER: DATE, PREPRINT INFO, CODE REPOSITORY
  // ---------------------------------------------------------------------------
  // Displays date, publication status, and repository link (if provided).
  if date != none and published != none and code != none {
    align(left)[#block(inset: (bottom: 1em))[
      #text(weight: "bold", size: 0.8em)[#date]
      #h(1em) // Small gap between date and preprint statement
      #text(size: 0.8em)[#published]
      #linebreak()
      #text(size: 0.8em)[#code]
    ]]
  } else if date != none and published != none {
    align(left)[#block(inset: (bottom: 1em))[
      #text(weight: "bold", size: 0.8em)[#date]
      #h(1em) // Small gap between date and preprint statement
      #text(size: 0.8em)[#published]
    ]]
  } else if date != none {
    align(left)[#block(inset: (bottom: 1em))[
      #text(weight: "bold", size: 0.8em)[#date]
    ]]
  }

  // ---------------------------------------------------------------------------
  // TITLE AND SUBTITLE BLOCKS
  // ---------------------------------------------------------------------------
  if title != none {
    v(2cm)
    align(center)[#block(inset: (bottom: 1em))[
        #text(weight: "bold", size: 1.4em)[#title]
    ]]
  }

  if subtitle != none{
    align(center)[#block(inset: (bottom: 1em))[
        #text(weight: "bold", size: 1.4em)[#subtitle]
    ]]
  }

  // ---------------------------------------------------------------------------
  // AUTHOR BLOCKS
  // ---------------------------------------------------------------------------
  // Two main modes:
  //   (1) `block-author` enabled  → authors printed in a grid (name, dept, email)
  //   (2) `block-author` disabled → inline (common for journal manuscripts)
  if authors != none and block-author != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      inset: (bottom: 1em),
      ..authors.map(author => align(center, {
        text(weight: "bold", author.name)
        // Corresponding author footnote
        if "corresponding" in author {
            if correspondence != none {
                footnote(correspondence, numbering: "*")
                counter(footnote).update(n => n - 1)
            }
        }
        // Optional ORCID link
        if "orcid" in author [
            #link("https://orcid.org/" + author.orcid)[
                #box(height: 9pt, image(bytes(orcid_svg)))
                ]
        ]
        // Department and email in smaller text
        set text(size: 0.8em)
        if author.department != none [
            #show ",": linebreak()
            \ #author.department
        ]
        if "email" in author [
            \ #link("mailto:" + to-string(author.email))
        ]
      }))
      )
      // --- Inline author layout ---
      // Personally I find this useful when there are 4+ authors as the grid
      // system above gets messy
  } else if authors != none {
     // First line: Author names with subscripts + ORCID
    align(center, {
      authors.map(author => {
        author.name
        h(1pt)
        super(author.subscript)
        if "corresponding" in author {
            if correspondence != none {
                footnote(correspondence, numbering: "*")
                counter(footnote).update(n => n - 1)
            }
        }
        if "orcid" in author [
            #link("https://orcid.org/" + author.orcid)[
                #box(height: 9pt, image(bytes(orcid_svg)))
                ]
        ]
      }).join(", ", last: " and ")
    })
    // Second line: unique affiliations
    align(center, {
      authors
      .map(author => (author.subscript, author.department))
      .dedup()
      .map(pair => {
        super(pair.at(0))
        h(1pt)
        pair.at(1)
      }).join("\n")
    })
  }

  // ---------------------------------------------------------------------------
  // ABSTRACT AND KEYWORDS
  // ---------------------------------------------------------------------------
  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[Abstract] #h(1em) #abstract
    ]
  }

  if keywords != none {
    v(-3.5em) // Brining the keywords closer to the abstract
    align(left)[#block(inset: 2em)[
      *Keywords*: #keywords.join(" • ")
    ]]
  }

  // ---------------------------------------------------------------------------
  // TABLE OF CONTENTS (optional)
  // ---------------------------------------------------------------------------
  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth
    );
    ]
  }

  // ---------------------------------------------------------------------------
  // MAIN BODY CONTENT
  // ---------------------------------------------------------------------------
  // Display document body either as a single column or multiple columns.
  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

// -----------------------------------------------------------------------------
// GLOBAL TABLE STYLE OVERRIDE
// -----------------------------------------------------------------------------
// Applies to all `table` blocks within the document by default.
// Removes outer borders and adds consistent padding.
#set table(
  inset: 6pt,
  stroke: none
)
#import "@preview/mitex:0.2.4": *

#set page(
  paper: "us-letter",
  margin: (x: 1.25in, y: 1.25in),
  numbering: "1",
)

// What to pull from the YAML file
#show: doc => shifu-article(
  title: [Goodness of fit diagnostics for Markov models],
  authors: (
    ( name: "Cormac Monaghan",
      last: "Monaghan",
          department: [Maynooth University],
      subscript: [1],
              email: [cormacmonaghan\@protonmail.com],
              orcid: "0000-0001-9012-3060",
            corresponding: true,
          ),
    ( name: "Idemauro Antonio Rodrigues de Lara",
      last: "Lara",
          department: [University of São Paulo],
      subscript: [2],
                  orcid: "0000-0002-1172-9855",
              ),
    ( name: "Rafael de Andrade Moral",
      last: "Andrade Moral",
          department: [Maynooth University],
      subscript: [1],
                  orcid: "0000-0002-0875-3563",
              ),
    ( name: "Joanna McHugh Power",
      last: "Power",
          department: [Maynooth University],
      subscript: [1],
                  orcid: "0000-0002-7387-3107",
              ),
    ),
  date: [Thursday, November 27, 2025],
  sectionnumbering: none,
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  linestretch: 1,
  linkcolor: "\#800000",
  doc,
)

= Introduction
<introduction>
Dementia is a neurogenerative disease characterized by progressive deterioration in cognitive ability @prince2013a. Mathematically, this progression can be represented as a sequence of $K$ mutually exclusive states such that $S = { 1 \, 2 \, dots.h K }$ beginning with a preclinical or asymptomatic period, transitioning through an intermediate stage such as mild cognitive impairment (MCI), and culminating in dementia @sanz-blasco2022. Understanding these transitions is central to epidemiological forecasting, intervention evaluation, and the design of health and social care services.

Because progression unfolds over time and involves movement between discrete, observable states, Markov models have become a widely used methodological tool for studying dementia trajectories @costa2023@salazar2007@sanz-blasco2022@wei2014@williams2020@yu2013a. Within these models, the cognitive state of individual $i$ at time $t$, denoted $Y_(i \, t) in S$, is represented as a stochastic process governed by the Markov property @zhang2010: #math.equation(block: true, numbering: "(1)", [ $ P (Y_(i \, t + 1) = k #h(0em) \| Y_(i \, t) = j \, Y_(i \, t - 1) = j_(t - 1) \, dots.h Y_(i 0) = j_0) = P (Y_(i \, t + 1) = k #h(0em) \| #h(0em) Y_(i \, t) = j) . $ ])<eq-markov-property>

This property asserts that the probability of transitioning from state $j$ to state $k$ depends only on the current state $Y_(i \, t)$ and not on the full history of preceding states ${ Y_(i \, t - 1) \, dots.h \, Y_(i 0) }$. Under this assumption, Markov models offer a flexible framework for estimating transition probabilities $p_(j k)$, describing the likelihood of movement from state $j$ to state $k$ over a fixed time interval.

Despite their widespread use, several practical challenges arise when applying Markov models in dementia research. Firstly, Markov models can be formulated in either continuous or discrete time, but many longitudinal cohort studies collect observations at discrete intervals (e.g., annual or biennial assessments). Although R @R2025 provides a well-developed ecosystem for fitting continuous-time Markov models @jackson2011@ucar2019, dedicated tools for discrete-time Markov models are less developed. Existing packages such as `DTMCPack` @nicholson2013 and `markovchain` @spedicato2017 are useful but do not support covariate-dependent discrete-time transitions. Consequently, a common approach is to estimate the transition probabilities using a multinomial logistic regression model (see #cite(<spackman2012>, form: "prose") as an example), treating the state at time $t + 1$ as a categorical outcome conditional on the state at time $t$. Within this framework, the transition probabilities from state $j$ are modeled as: #math.equation(block: true, numbering: "(1)", [ $ l o g (frac(p_(j k) (upright(bold(z_(i \, t)))), p_(j 1) (upright(bold(z_(i \, t)))))) = alpha_(j k) + upright(bold(z))_(i \, t)^(⊺) beta_(j k) #h(2em) k = 2 \, dots.h \, K \, $ ])<eq-multinomial-formula>

where $p_(j 1)$ is the reference probability (e.g., remaining in state $j$), $alpha_(j k)$ is a state-pair specific intercept, and $beta_(j k)$ is a vector of coefficients for covariates $upright(bold(z))_(i \, t)$ (of which $Y_(i \, t)$ is inclusive).

However, while estimating covariate-dependent transition probabilities is straightforward using multinomial models, such that for a non-reference category, #math.equation(block: true, numbering: "(1)", [ $ p_(j k) (upright(bold(z))_(i \, t)) = frac(exp (eta_(i \, t)^((k))), 1 + sum_(ell = 2)^K exp (eta_(i \, t)^((ell)))) \, #h(2em) upright("for ") k = 2 \, 3 $ ])<eq-non-reference-probability>

and for the reference category, #math.equation(block: true, numbering: "(1)", [ $ p_(j 1) (upright(bold(z))_(i \, t)) = frac(1, 1 - sum_(j = 1)^(J - 1) exp (eta_(i \, t)^((j)))) $ ])<eq-reference-probability>

where $eta_(i \, t)^((k)) = alpha_(j k) + upright(bold(z))_(i \, t)^tack.b beta_(j k)$, diagnostic analysis and evaluating the goodness of fit of these Markov models remains an methodological challenge and is subject to ongoing research @araripe2024. Unlike conventional regression frameworks, where a direct comparison is made between observed $(y)$ and predicted $(hat(y))$ values, the output of a Markov model is a set of transition probabilities. For a model with the $K$ states these probabilities are represented by a $K times K$ transition probability matrix $upright(bold(P))$, where each element $p_(j k)$ is the probability of transitioning from state $j$ at time $t$ to state $k$ at time $t + 1$: $ P = mat(delim: "(", p_11, p_12, dots.h, p_(1 K); p_21, p_22, dots.h, p_(2 K); dots.v, dots.v, dots.down, dots.v; p_(K 1), p_(K 2), dots.h, p_(K K)) . $

Since an observed transition is a stochastic realization from this probability distribution, there is no direct analogue to prediction error or residuals that cleanly links observed transitions to model-implied transition mechanisms. Existing methods, such as likelihood-ratio tests, information criteria, or simulation envelopes, provide valuable but partial insights. They often fail to provide a comprehensive, quantitative measure of how well the aggregate transition structure of the fitted transition probability matrix replicates the empirical dynamics observed within the cohort data.

== Distance metrics
<distance-metrics>
To address this gap, a natural approach is to compare the empirical transition structure observed in the data with the model-implied transition structure produced by the fitted model. Let $upright(bold(P))$ denote the empirical transition matrix estimated directly from the observed transitions, and let $hat(upright(bold(P)))$ denote the corresponding transition matrix estimated by fitting a Markov model. The central question then becomes: "How different is $hat(upright(bold(P)))$ from $upright(bold(P))$?". In univariate models, a simple way of measuring this difference is by calculating a residual, which typically relies on the signed unidimensional distance between an observation and a fitted value (e.g., $y - hat(y)$). However, determining the best way to measure a distance between two matrices is less trivial.

Distance-based metrics provide a principled way to quantify discrepancies between two transition matrices. These metrics treat the transition matrix as a $K times K$ object whose structure can be assessed globally, rather than focusing on individual probabilities in isolation. Let $upright(bold(D)) = upright(bold(P)) - hat(upright(bold(P)))$ denote the matrix of element-wise differences. Several matrix norms and divergence measures can then be used to summarize the magnitude of discrepancy, each emphasizing different structural features of the transition process. For the purpose of this paper we focus on six of these distance measures which are summarized in #ref(<tbl-metrics>, supplement: [Table]). In addition, we compare these measures against current benchmark information criteria (AIC and BIC) within simulation studies whose complete setup is explained in the Methods section.

#figure([
#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "1_0": 0, "2_0": 0, "3_0": 0, "4_0": 0, "5_0": 0, "6_0": 0, "7_0": 0, "8_0": 0, "0_1": 1, "0_0": 2
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: left,),
    (bold: true, align: center,),
    (bold: true, align: left,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 2, stroke: 0.1em + black),
 table.hline(y: 9, start: 0, end: 2, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[Metric], [Formula],
    ),
    // tinytable header end

    // tinytable cell content after
[Frobenius norm], [#mitex(`||\mathbf{D}||_F = \sqrt{\sum_{j=1}^{K}\sum_{k=1}^{K} d_{j,k}^2}`)],
[Manhattan distance], [#mitex(`\sum_{j=1}^{K}\sum_{k=1}^{K} |d_{j,k}|`)],
[Maximum absolute error], [#mitex(`\max_{j,k} |d_{j,k}|`)],
[Root mean squared error], [#mitex(`\sqrt{ \frac{1}{K^2} \sum_{j=1}^{K}\sum_{k=1}^{K} d_{j,k}^2 }`)],
[Correlation dissimilarity], [#mitex(`1 - \text{corr}(\text{vec}(\mathbf{P}), \text{vec}(\hat{\mathbf{P}}))`)],
[Kullback-Leibler divergence], [#mitex(`\sum_{j=1}^{K}\sum_{k=1}^{K} (p_{j,k} + \epsilon) \, \log\!\left(\frac{p_{j,k}}{\hat{p}_{j,k}}\right)`)],
[Akaike information criterion], [#mitex(`2k - 2\text{ln}(\hat{\ell})`)],
[Bayesian information criterion], [#mitex(`-2\text{ln}(\hat{\ell}) + k\text{ln}(n)`)],

    // tinytable footer after

    table.footer(
      repeat: false,
      // tinytable notes after
    table.cell(align: left, colspan: 2, text([K represents the total number of categories; corr(x, y) = is the Pearson correlation function; vec(⋅) is an operator that converts a matrix into a vector by stacking its columns; 𝜃 is the number of model parameters; ℓ is the model’s log-likelihood; 𝑛 is the sample size. Additionally, for the Kullback–Leibler divergence, a small constant ε = 1e-10 is added to each matrix entry to avoid undefined logarithms when probabilities are zero.])),
    ),
    

  ) // end table

  ]) // end align

] // end block
], caption: figure.caption(
position: top, 
[
Distance based metrics and information crietia used in study
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-metrics>


#pagebreak()
= Methods
<methods>
== Simulation study aims
<simulation-study-aims>
We designed a simulation study to evaluate the performance of different matrix-based distance metrics in assessing the goodness-of-fit of discrete-time Markov models for dementia progression. The primary aim was to determine whether these metrics can reliably detect misspecification in multinomial logistic regression models used to estimate transition probabilities. The study follows best practices for simulation reporting using the ADEMP framework @morris2019@siepe2024.

== Data generating mechanisms
<data-generating-mechanisms>
=== Markov process structure
<markov-process-structure>
We simulated data for $N = 10 \, 000$ individuals across $t = 3$ waves. Consistent with typical dementia progression, the data was simulated for a process with $K = 3$ mutually exclusive states such that $S in { 1 \, 2 \, 3 }$, representing a pre-clinical state (1), mild cognitive impairment (2), and dementia (3). For each individual $i in { 1 \, dots.h \, N }$ and discrete time point $t in { 1 \, dots.h \, T }$, the true state at time $t$ is denoted as $Y_(i \, t) in S$. Under a first-order Markov process, transitions between states satisfy the Markov property (#ref(<eq-markov-property>, supplement: [Equation])).

=== Covariate specification
<covariate-specification>
We generated a set of time-invariant and time-varying covariates designed to mimic plausible risk factors observed in ageing cohort studies @sonnega2011. Each covariate was chosen to reflect either a common demographic factor, a behavioural or psychological construct, or generic background variability that may influence cognitive transitions @monaghan2026.

- $upright(bold(x_1))$: A binary covariate drawn from a Bernoulli distribution with probability of success $0.5$. Within the simulation, this variable represents a binary demographic attribute such as gender.
- $upright(bold(x_2))$: A continuous covariate drawn from a normal distribution $cal(N) (70 \, 25)$ and then rounded to the nearest whole number. Within the simulation, this variable represents age.
- $upright(bold(x_3))$: A rounded continuous covariate from a normal distribution $cal(N) (25 \, 225)$, truncated to the interval $[0 \, 60]$ such that values above or below the interval are set to the nearest value within the interval (i.e., 0 or 60) and then rounded to the nearest whole number. Within the simulation, this variable represents psychological or behavioural assessments (e.g., memory tests or psychosocial scales) with fixed bounded scores.
- $upright(bold(x_4 \, x_5))$: Continuous noise variables, drawn from a uniform distribution $cal(U) (0 \, 1)$.

Four of these covariates evolved over time to introduce time-varying confounding: $ x_(2 i \, t) & = x_(2 i 0) + (t - 1) times 2\
x_(3 i \, t) & = min #scale(x: 180%, y: 180%)[\(] 60 \, max #scale(x: 120%, y: 120%)[\(] 0 \, x_(3 i \, (t - 1)) + epsilon.alt_(i \, t)^((3)) #scale(x: 120%, y: 120%)[\)] #scale(x: 180%, y: 180%)[\)] &  & epsilon.alt_(i \, t)^((3)) tilde.op N (5 \, 4)\
x_(4 i \, t) & = min #scale(x: 180%, y: 180%)[\(] 1 \, max #scale(x: 120%, y: 120%)[\(] 0 \, x_(4 i \, t - 1) + epsilon.alt_(i \, t)^((4)) #scale(x: 120%, y: 120%)[\)] #scale(x: 180%, y: 180%)[\)] &  & epsilon.alt_(i \, t)^((4)) tilde.op cal(U) (0 \, 0.062)\
x_(5 i \, t) & = min #scale(x: 180%, y: 180%)[\(] 1 \, max #scale(x: 120%, y: 120%)[\(] 0 \, x_(5 i \, t - 1) + epsilon.alt_(i \, t)^((5)) #scale(x: 120%, y: 120%)[\)] #scale(x: 180%, y: 180%)[\)] &  & epsilon.alt_(i \, t)^((5)) tilde.op cal(U) (0 \, 0.062) \, $

where $t in { 1 \, 2 \, 3 }$ indexes follow-up waves. The full covariate vector for individual $i$ at time $t$ was $upright(bold(z))_(i \, t) = (x_(1 i \, t) \, #h(0em) x_(2 i \, t) \, #h(0em) x_(3 i \, t) \, #h(0em) x_(4 i \, t) \, #h(0em) x_(5 i \, t))$.

#pagebreak()
=== Simulation scenarios
<simulation-scenarios>
We evaluated three data-generating mechanisms (DGMs) of increasing complexity. In all scenarios, transition probabilities were governed by a multinomial logistic model (#ref(<eq-multinomial-formula>, supplement: [Equation])) with state 1 as the reference category. The corresponding transition probability for each non-reference category was calculated using #ref(<eq-non-reference-probability>, supplement: [Equation]) and for the reference category using #ref(<eq-reference-probability>, supplement: [Equation]). Additionally, , all true value parameters vectors for scenarios S1, S2, and S3 $(theta_(S 1) \, theta_(S 2) \, theta_(S 3) \, upright("respectively"))$ were derived from prior empirical research investigating behavioral correlates of dementia transitions @livingston2024a@monaghan2026.

In scenario 1, transitions depended solely on a set of individual covariates $upright(bold(X))_(i \, t) = (x_(1 i t) \, x_(2 i t) \, x_(3 i t))$, with no effect of the previous state. Within this scenario, the linear predictor was defined as: $ eta_(i \, t)^((k)) = alpha_k + upright(bold(X))_(i t)^(⊺) beta_k \, $

with true value parameters set as: $ theta_(S 1) & = (alpha_k \, #h(0em) beta_(1 k) \, #h(0em) beta_(2 k) \, #h(0em) beta_(3 k) \, #h(0em) : k = 2 \, 3)\
alpha & = (- 5.152 \, - 5.402)\
beta & = (0.008 \, - 0.034 \, 0.036 \, 0.017 \, 0.028 \, 0.045) . $

In scenario 2, transitions depended additively on both the covariates $upright(bold(X))_(i \, t) = (x_(1 i t) \, x_(2 i t) \, x_(3 i t))$ and the individual's previous state $Y_(i t)$. Within this scenario, the linear predictor was defined as: $ eta_(i \, t)^((k)) & = alpha_k + upright(bold(X))_(i t)^(⊺) beta_k + upright(bold(S))_(i t)^(⊺) gamma_k \, $

where $upright(bold(S))_(i t)$ is an indicator variable. The true value parameters were set as: $ theta_(S 2) & = (alpha_k \, #h(0em) beta_(1 k) \, #h(0em) beta_(2 k) \, #h(0em) beta_(3 k) \, gamma_(1 k) \, #h(0em) gamma_(2 k) \, #h(0em) : k = 2 \, 3)\
alpha & = (- 5.370 \, - 7.907)\
beta & = (0.02 \, - 0.011 \, 0.036 \, 0.039 \, 0.022 \, 0.016 \, 1.711 \, 2.790)\
gamma & = (- 0.776 \, 20.914) . $

Finally, in scenario 3, transitions depended both on the covariates $upright(bold(X))_(i \, t) = (x_(1 i t) \, x_(2 i t) \, x_(3 i t))$ and the individual's previous state $Y_(i t)$ along with their corresponding interactions. Within this scenario, the linear predictor was defined as: $ eta_(i \, t)^((k)) = alpha_k + upright(bold(X))_(i t)^(⊺) beta_k + upright(bold(S))_(i t)^(⊺) gamma_k + (upright(bold(X))_(i t) times.circle upright(bold(S))_(i t))^(⊺) delta_k \, $ with true value parameters defined as: $ theta_(S 3) & = (alpha_k \, #h(0em) beta_(1 k) \, #h(0em) beta_(2 k) \, #h(0em) beta_(3 k) \, gamma_(1 k) \, #h(0em) gamma_(2 k) \, #h(0em) delta_(1 k) \, #h(0em) delta_(2 k) \, #h(0em) delta_(3 k) \, #h(0em) delta_(4 k) \, #h(0em) #h(0em) delta_(5 k) \, #h(0em) delta_(6 k) \, : k = 2 \, 3)\
alpha & = (- 5.885 \, - 10.613)\
beta & = (0.102 \, 0.615 \, 0.043 \, 0.076 \, 0.019 \, - 0.002)\
gamma & = (3.845 \, 7.901 \, - 0.638 \, 12.753)\
delta & = (- 0.292 \, - 1.019 \, - 0.474 \, - 1.268 \, - 0.031 \, - 0.072 \, 0.093 \, 0.1 \, 0.008 \, 0.029 \, - 0.082 \, - 0.021) . $

== Estimation methods
<estimation-methods>
=== Model fitting
<model-fitting>
For each simulated dataset, we first partitioned the data into training $(80 %)$ and test $(20 %)$ sets. Model parameters were estimated exclusively on the training data, while all performance evaluations were conducted on held-out test data. For each training set, we fitted 15 multinomial logistic regression models using the #emph[nnet] package @venables2022 across 4 different sample sizes ${ 100 \, 250 \, 1000 \, 5000 }$. These models were grouped into three "families", corresponding to the level of model misspecification relative to the true DGM.

+ Base Models (Ignoring Markov Property)

#math.equation(block: true, numbering: "(1)", [ $ upright("Null:") & quad Y_(t + 1) tilde.op 1\
upright("Reduced 1:") & quad Y_(t + 1) tilde.op x 1\
upright("Reduced 2:") & quad Y_(t + 1) tilde.op x 1 + x 2\
upright("True:") & quad Y_(t + 1) tilde.op x 1 + x 2 + x 3 quad upright("(Exact DGM for Scenario 1)")\
upright("Overfit:") & quad Y_(t + 1) tilde.op x 1 + x 2 + x 3 + x 4 + x 5 $ ])<eq-base-models>

#block[
#set enum(numbering: "1.", start: 2)
+ Additive Models (With State Dependence)
]

#math.equation(block: true, numbering: "(1)", [ $ upright("Null:") & quad Y_(t + 1) tilde.op Y_t\
upright("Reduced 1:") & quad Y_(t + 1) tilde.op x 1 + Y_t\
upright("Reduced 2:") & quad Y_(t + 1) tilde.op x 1 + x 2 + Y_t\
upright("True:") & quad Y_(t + 1) tilde.op x 1 + x 2 + x 3 + Y_t quad upright("(Exact DGM for Scenario 2)")\
upright("Overfit:") & quad Y_(t + 1) tilde.op x 1 + x 2 + x 3 + x 4 + x 5 + Y_t $ ])<eq-add-models>

#block[
#set enum(numbering: "1.", start: 3)
+ Multiplicative Models (With Interactions)
]

#math.equation(block: true, numbering: "(1)", [ $ upright("Null:") & quad Y_(t + 1) tilde.op Y_t\
upright("Reduced 1:") & quad Y_(t + 1) tilde.op x 1 \* Y_t\
upright("Reduced 2:") & quad Y_(t + 1) tilde.op (x 1 + x 2) \* Y_t\
upright("True:") & quad Y_(t + 1) tilde.op (x 1 + x 2 + x 3) \* Y_t quad upright("(Exact DGM for Scenario 3)")\
upright("Overfit:") & quad Y_(t + 1) tilde.op (x 1 + x 2 + x 3 + x 4 + x 5) \* Y_t $ ])<eq-mult-models>

=== Model-based transition probabilities
<model-based-transition-probabilities>
To evaluate how well each fitted model $cal(M)$ captured the underlying transition dynamics, we generated model-based transition probability matrices via a two step procedure. In the first step, an augmented version of the test dataset was created enabled the model to predict transitions from all possible previous states. For each individual $i$ at each time point $t$, three augmented rows were created corresponding to the three possible previous states $j in { 1 \, 2 \, 3 }$. This ensured that the fitted model could generate predicted probabilities $ P (Y_(t + 1) = k #h(0em) \| #h(0em) Y_t = j \, upright(bold(z))_(i \, t)) $

for every $j$, regardless of the individual's actual previous state in the data.

In the second step, we derived the transition behaviour implied by each model by predicting next-state transitions using the model's estimated parameters. For each model $cal(M)$, the estimated coefficients $hat(theta)^(cal(M))$ were used to compute predicted probabilities for all possible state transitions $(j arrow.r k)$.

== Performance Measures
<performance-measures>
The core of our evaluation involved comparing the empirical transition matrix from the test dataset to the model-implied transition matrix. Let $upright(bold(P))_(i \, t)$ be the empirical transition matrix for individual $i$ at time $t$ estimated by tabulating state transitions $(Y_(i \, t) \, Y_(i \, t + 1))$ from the test dataset. Additionally, let $hat(upright(bold(P)))_(i \, t)$ be the model-implied transition matrix. For individual $i$ at time $t$ with a covariate pattern $upright(bold(z))_(i \, t)$ this is a $K times K$ matrix where the entry $j \, k$ is the model's predicted probability $P (Y_(t + 1) = k #h(0em) \| #h(0em) Y_t = j \, upright(bold(z))_(i \, t))$. We defined the difference matrix as $upright(bold(D))_(i \, t) = upright(bold(P))_(i \, t) - hat(upright(bold(P)))_(i \, t)$ and quantify the discrepancy using the distance metrics defined in #ref(<tbl-metrics>, supplement: [Table]).

= Results
<results>
Initially we present an exploratory analysis of the data from each scenario using contingency tables. #ref(<tbl-transitions>, supplement: [Table]) summarizes the unconditional transitions and transition probabilities between each cognitive state across the scenarios.

#figure([
#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "0_0": 0, "0_1": 0, "1_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "7_1": 0, "8_1": 0, "9_1": 0, "11_1": 0, "12_1": 0, "13_1": 0, "0_2": 0, "1_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "7_2": 0, "8_2": 0, "9_2": 0, "11_2": 0, "12_2": 0, "13_2": 0, "0_3": 0, "1_3": 0, "3_3": 0, "4_3": 0, "5_3": 0, "7_3": 0, "8_3": 0, "9_3": 0, "11_3": 0, "12_3": 0, "13_3": 0, "0_4": 0, "1_0": 1, "3_0": 1, "4_0": 1, "5_0": 1, "7_0": 1, "8_0": 1, "9_0": 1, "11_0": 1, "12_0": 1, "13_0": 1, "2_1": 2, "6_1": 2, "10_1": 2, "2_2": 2, "6_2": 2, "10_2": 2, "2_3": 2, "6_3": 2, "10_3": 2, "2_0": 3, "6_0": 3, "10_0": 3, "2_4": 3, "6_4": 3, "10_4": 3
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: center,),
    (align: left,),
    (bold: true, align: center,),
    (bold: true, align: left,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    column-gutter: 5pt,
    columns: (auto, auto, auto, auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 1, end: 4, stroke: 0.05em + black), table.hline(y: 1, start: 1, end: 4, stroke: 0.05em + black),
 table.hline(y: 2, start: 0, end: 5, stroke: 0.1em + black),
 table.hline(y: 6, start: 0, end: 5, stroke: 0.1em + black),
 table.hline(y: 10, start: 0, end: 5, stroke: 0.1em + black),
 table.hline(y: 14, start: 0, end: 5, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[ ], table.cell(colspan: 3, align: center)[Current state (t)], [ ],
[Previous state (t-1)], [1], [2], [3], [Total],
    ),
    // tinytable header end

    // tinytable cell content after
table.cell(colspan: 5)[Base simulation],
[1], [12856 \ (0.64)], [2456 \ (0.12)], [847 \ (0.04)], [16159],
[2], [2155 \ (0.11)], [498 \ (0.02)], [204 \ (0.01)], [2857],
[3], [750 \ (0.04)], [167 \ (0.01)], [67 \ (0.003)], [984],
table.cell(colspan: 5)[Additive simulation],
[1], [13817 \ (0.69)], [1996 \ (0.10)], [192 \ (0.01)], [16005],
[2], [1657 \ (0.08)], [2606 \ (0.04)], [162 \ (0.01)], [2606],
[3], [120 \ (0.01)], [55 \ (0.003)], [1214 \ (0.06)], [1389],
table.cell(colspan: 5)[Multiplicative simulation],
[1], [14030 \ (0.70)], [1921 \ (0.10)], [166 \ (0.01)], [16117],
[2], [1602 \ (0.08)], [712 \ (0.04)], [167 \ (0.01)], [2481],
[3], [102 \ (0.01)], [61 \ (0.003)], [1239 \ (0.06)], [1402],

    // tinytable footer after

  ) // end table

  ]) // end align

] // end block
], caption: figure.caption(
position: top, 
[
Unconditional transitions and transition probabilities across scenarios.
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-transitions>


To evaluate the ability of each distance metric to identify the true data-generating model, we examined the proportion of simulation repetitions in which each model $cal(M)$ was ranked as the best-fitting (#ref(<fig-replications>, supplement: [Figure])).

#pagebreak()
== Small sample sizes
<small-sample-sizes>
For the smallest sample size $(n = 100)$ clear differences emerged between distance-based metrics and likelihood-based information criteria. Across both additive and multiplicative generative scenarios, the Manhattan distance and the Kullback--Leibler divergence identified the true model at rates comparable to, and in several cases exceeding, those of AIC and BIC. As shown in #ref(<fig-replications>, supplement: [Figure]) both metrics maintained a non-trivial probability of selecting the true model even under increased model complexity, whereas the information criteria frequently favoured simpler alternatives. This relative robustness at small $n$ suggests that the Manhattan distance and KL divergence are less sensitive to the sampling variability that destabilises likelihood-based criteria in sparse data settings. In contrast, the remaining metrics (e.g., Frobenius norm, RMSE, correlation dissimilarity) showed mixed performance, often favoring overfitted or reduced models.

At $n = 250$, AIC showed modest improvement, particularly for simpler generative mechanisms. However, it continued to misidentify the true model under increased complexity, most notably in the multiplicative scenarios. BIC remained unreliable across all scenarios, strongly penalizing model complexity and frequently selecting the null model. In contrast, the Manhattan distance continued to demonstrate comparatively stable recovery of the true model across repetitions, and most clearly outperformed AIC in the more complex additive and multiplicative settings. The Kullback--Leibler divergence similarly outperformed both AIC and BIC up to, but not including, the most complex multiplicative scenario.

== Moderate sample sizes
<moderate-sample-sizes>
With a further increase in sample size $(n = 1000)$, the performance of AIC improved substantially. As shown by #ref(<fig-replications>, supplement: [Figure]) AIC correctly identified the true model in over approximately $90 %$ of repetitions across all generative scenarios. However, BIC continued to exhibit systematic underfitting, performing well only in the simplest generative scenario and frequently favoring the null model in both the additive and multiplicative scenarios.

The distance-based metrics displayed a more gradual improvement with increasing sample size. Most notably, the Kullback--Leibler divergence performed nearly equivalently to AIC across all scenarios, indicating strong sensitivity to discrepancies in the underlying transition structure. The Manhattan distance also remained informative, ranking the true model as best fitting in approximately half of all repetitions across scenarios. Although this performance was weaker than that of AIC, it did not deteriorate with increasing model complexity, suggesting that this metric captures aspects of model misspecification that are not fully reflected in likelihood-based criteria. The remaining distance measures continued to show heterogeneous behaviour, alternating between overfitting and instability across repetitions.

== Large sample sizes
<large-sample-sizes>
At the largest sample size $(n = 5000)$, both AIC and BIC exhibited near-perfect performance, identifying the true model in essentially all simulation repetitions across all generative scenarios. This convergence confirms that, when sufficient data are available, traditional likelihood-based information criteria are adequate for reliable model recovery. In contrast, the relative performance of the distance-based metrics was largely comparable to that observed at $n = 1000$ showing limited additional gains with further increases in sample size. Notably, however, the Kullback--Leibler divergence mirrored the behaviour of AIC and BIC, identifying the true model in nearly all repetitions even under the most complex multiplicative generative scenario.

#figure([
#box(image("manuscript_files/figure-typst/fig-replications-1.svg", width: 100.0%))
], caption: figure.caption(
position: bottom, 
[
Proportion of simulation repetitions in which each candidate model was ranked as best fitting across distance metrics and information criteria. Each stacked bar corresponds to a model class, with segment heights representing the percentage of repetitions in which each sub-model was selected as the best fitting. AIC = Akaike information criterion; BIC = Bayesian information criterion.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-replications>


#pagebreak()
= Case study
<case-study>
We illustrate the proposed goodness-of-fit assessment framework using data from the United States Health and Retirement Study (HRS; #cite(<sonnega2011>, form: "prose");). The HRS is a nationally representative longitudinal panel study administered by the Institute for Social Research at the University of Michigan, which follows American adults aged 50 years and older. Since its inception in 1992, the HRS has collected biennial data on participants' health, socioeconomic circumstances, and cognitive functioning, making it a widely used resource for studying cognitive ageing and dementia trajectories. For the purpose of this illustration, we focus on three of these biennial waves from 2018 - 2022, yielding a sample of $n = 10 \, 895$ respondents.

Cognitive function in the HRS is measured using a battery of assessments adapted from the Telephone Interview for Cognitive Status (TICS; #cite(<fong2009>, form: "prose");). These assessments include immediate and delayed noun free-recall tasks to evaluate episodic memory, a serial sevens subtraction task to assess working memory, and a backward counting task to capture mental processing speed. Based on these assessments, #cite(<crimmins2011>, form: "prose") developed a validated 27-point cognitive scale along with established cut-off points to classify respondents' cognitive status. Following this classification scheme, respondents scoring between 12 and 27 were classified as having normal cognition, scores between 7 and 11 indicated mild cognitive impairment (MCI), and scores between 0 and 6 were classified as dementia.

From the available HRS covariates, we selected three predictors that are commonly examined in studies of cognitive decline @livingston2024a: sex ($x 1$), age ($x 2$), and a self memory rating ($x 3$). To mirror the design of the simulation study and to assess the ability of the proposed distance metrics to distinguish informative predictors from noise, we additionally simulated two non-informative covariates from a Uniform distribution, such that ${ x 4 ; x 5 } tilde.op cal(U) (0 \, 1)$.

Using this covariate set, we followed the same model-fitting and evaluation procedure as in the simulation study. Specifically, the data were split into training (80%) and test (20%) sets, the 15 multinomial logistic regression models defined in #ref(<eq-base-models>, supplement: [Equation]), #ref(<eq-add-models>, supplement: [Equation]), and #ref(<eq-mult-models>, supplement: [Equation]) were fitted, and observed transition probabilities $upright(bold(P))_(i \, t)$ were compared to predicted probabilities $hat(upright(bold(P)))_(i \, t)$. #ref(<fig-case-study>, supplement: [Figure]) summarises the relative performance of each model across the different goodness-of-fit measures.

Consistent with the simulation results (see #ref(<fig-replications>, supplement: [Figure])), both the Manhattan distance and the Kullback--Leibler divergence most frequently identified the model containing the three substantively meaningful predictors $(x 1 \, x 2 \, x 3)$ in approximately $50 %$ of repetitions in both the base and additive scenarios. In the more complex multiplicative scenario, differences between the distance metrics became more pronounced. While the Manhattan distance increasingly favoured the model excluding the simulated noise covariates as sample size grew, the Kullback--Leibler divergence required substantially larger samples $(n = 1000)$ before consistently identifying the model containing only the non-simulated predictors (mirroring that of the simulation study).

#figure([
#box(image("manuscript_files/figure-typst/fig-case-study-1.svg", width: 100.0%))
], caption: figure.caption(
position: bottom, 
[
Proportion of case study repetitions in which each candidate model was ranked as best fitting across distance metrics and information criteria. Each stacked bar corresponds to a model class, with segment heights representing the percentage of repetitions in which each sub-model was selected as the best fitting. AIC = Akaike information criterion; BIC = Bayesian information criterion.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-case-study>


= Discussion
<discussion>
This study evaluated a set of matrix-based distance metrics as tools for assessing goodness-of-fit in discrete-time Markov models estimated via multinomial logistic regression, with particular emphasis on applications to dementia progression modelling. Through a combination of simulation experiments and an empirical case study using HRS data, we demonstrate that distance-based comparisons of observed and model-implied transition matrices provide information that is complementary to, and in some settings more reliable than, traditional likelihood-based information criteria.

Across the simulation study, two distance metrics consistently exhibited strong performance: the Manhattan distance and the Kullback--Leibler divergence. Both exhibited strong and comparatively stable performance across a range of sample sizes and data-generating mechanisms, particularly under increased model complexity. Most notably, the Manhattan distance frequently outperformed AIC and BIC in small samples and in settings involving interaction effects or strong state dependence. This finding suggests that the Manhattan distance is comparatively robust to the sampling variability that can destabilise likelihood-based criteria in finite samples @emiliano2014.

The KL divergence displayed a complementary pattern. While less robust than the Manhattan distance in the smallest samples and most complex scenarios, its performance improved rapidly with increasing sample size. By moderate to large samples, KL closely mirrored the behaviour of AIC, and at the largest sample sizes it achieved near-perfect recovery of the true model even under the most complex multiplicative data-generating mechanisms. This aligns with the theoretical interpretation of KL divergence as a measure of information loss between true and fitted transition distributions @kullback1951.

As expected, the performance of traditional information criteria improved substantially with increasing sample size. In large samples, both AIC and BIC demonstrated near-perfect recovery of the true data-generating model, consistent with their well-established asymptotic properties. These results confirm that likelihood-based approaches remain appropriate and effective when data are abundant and model complexity is well supported.

However, the more gradual convergence of the distance-based metrics highlights an important conceptual distinction. Distance metrics do not aim to optimise predictive likelihood; instead, they assess how well a fitted model reproduces the empirical transition structure itself. Their continued sensitivity to structural discrepancies---even in larger samples---should therefore be viewed as a strength rather than a limitation. In applied settings, particularly in disease progression modelling, a model that fits well in likelihood terms may still produce implausible or distorted transition dynamics, a form of misspecification that distance-based diagnostics are well positioned to detect.

The empirical case study using HRS data further corroborated the simulation findings. Both the Manhattan distance and KL divergence tended to favour models containing substantively meaningful predictors of cognitive decline (sex, age, and self-rated memory), while remaining comparatively insensitive to the inclusion of simulated non-informative covariates. This behaviour is consistent with the intended role of the distance metrics: to distinguish meaningful signal in transition dynamics from noise introduced by irrelevant predictors.

Taken together, these findings suggest that matrix-based distance metrics---particularly the Manhattan distance and KL divergence---provide a valuable addition to the model evaluation toolkit for discrete-time Markov models. Rather than serving as replacements for AIC or BIC, these measures are best viewed as complementary diagnostics that foreground structural fidelity of transition dynamics. Their use is especially well suited to applied research contexts, such as dementia progression modelling, where the realism and interpretability of implied transitions are at least as important as predictive likelihood.

== Limitations and future directions
<limitations-and-future-directions>
Several limitations of the present study suggest directions for future research. First, although the proposed goodness-of-fit framework captures discrepancies in transition structure that are not reflected in likelihood-based criteria, the choice of distance metric remains application-dependent. Different metrics emphasize different aspects of the transition matrix (e.g., global versus state-specific discrepancies), and no single metric can be considered universally optimal. Future work could explore principled strategies for selecting or combining distance measures, potentially informed by substantive theory or decision-analytic considerations. Secondly, this simulation framework focuses on discrete-time, first-order Markov models. Extensions to higher-order Markov processes (e.g., continuous-time models, or hidden Markov models) represent promising avenues for future research (see #cite(<zeng2010>, form: "prose") as example).

== Conclusions
<conclusions>
Matrix-based distance metrics offer a principled and interpretable complement to likelihood-based criteria for assessing discrete-time Markov models. By directly comparing empirical transition matrices to those implied by fitted models, these measures provide diagnostic information that is particularly sensitive to structural misspecification. The strong finite-sample performance of the Manhattan distance and the favourable asymptotic behaviour of the Kullback--Leibler divergence highlight their practical utility, especially in applied longitudinal settings where accurate representation of transition dynamics is central to substantive inference.

#set bibliography(style: "files/apa.csl")

#set par(
  first-line-indent: 0em,
  hanging-indent: 0em,
  leading: 0.65em
)

#bibliography("references.bib", title: "References")

