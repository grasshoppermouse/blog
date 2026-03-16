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



#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  lang: "en",
  region: "US",
  font: "libertinus serif",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: "libertinus serif",
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)
  if title != none {
    align(center)[#block(inset: 2em)[
      #set par(leading: heading-line-height)
      #if (heading-family != none or heading-weight != "bold" or heading-style != "normal"
           or heading-color != black) {
        set text(font: heading-family, weight: heading-weight, style: heading-style, fill: heading-color)
        text(size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(size: subtitle-size)[#subtitle]
        }
      } else {
        text(weight: "bold", size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(weight: "bold", size: subtitle-size)[#subtitle]
        }
      }
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)

#set page(
  paper: "us-letter",
  margin: (x: 1.25in, y: 1.25in),
  numbering: "1",
)

#show: doc => article(
  title: [Review of #emph[Sex Is a Spectrum: The Biological Limits of the Binary] by Agustín Fuentes],
  authors: (
    ( name: [Ed Hagen],
      affiliation: [],
      email: [] ),
    ),
  date: [2025-08-22],
  sectionnumbering: "1.1.a",
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)

#quote(block: true)[
#strong[Sex Is a Spectrum offers a bold new paradigm for understanding the biology of sex….]

\- Marketing blurb on Amazon.com
]

Even as he voyaged on the Beagle, Darwin's social profile was rising. Letters he sent home were read at scientific societies in London, as were parts of his journal, and a prominent geologist gushed that "he will have a great name among the Naturalists of Europe" @darwin1835. Darwin's accounts of his voyage, published a few years after his return, further bolstered his reputation. Books on travel and exploration were popular at the time, and Darwin was apparently a fan, bringing several with him on his five year expedition @tallmadge1980. Darwin's #emph[The Voyage of the Beagle];, framed as a journey from the known to the unknown to bring back new knowledge, was very well received.

Darwin could have doubled down on his growing reputation as an explorer-scientist, giving talks and writing books on strange new worlds and the incredible diversity of the animals and peoples inhabiting them. Instead, pursuing a hint he dropped in #emph[The Voyage] (see #ref(<fig-finches>, supplement: [Figure])), he sought the causal principles underlying this diversity, giving us two: natural selection and its important variant, sexual selection. These two principles go a long way toward explaining the diversity of all life on earth.

#figure([
#box(image("finches.png"))
], caption: figure.caption(
position: bottom, 
[
"The most curious fact is the perfect gradation in the size of the beaks in the different species of Geospiza….Seeing this gradation and diversity of structure in one small, intimately related group of birds, one might really fancy that #strong[from an original paucity of birds in this archipelago, one species had been taken and modified for different ends];." Charles Darwin, The Voyage of the Beagle, 1845 (my emphasis)
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-finches>


Agustín Fuentes, an anthropologist at Princeton, whose previous book was #emph[Race, monogamy, and other lies they told you: Busting myths about human nature];, opens his new book, #emph[Sex is a Spectrum] (hereafter, SIAS), with a description of the strange physiology of the bluehead wrasse, a sequential hermaphrodite:

#quote(block: true)[
Imagine you are a fish called the bluehead wrasse, living off the coast of Florida. As you grow up, you, just like all to the other bluehead wrasse your age and size, develop one set of reproductive organs. You are what we'd call female, so you produce eggs. There is only one very large member of your group, and they are the group male, so produce sperm. But over the next few weeks you grow really fast, becoming the second-largest fish on your reef. Then the male gets eaten. Almost immediately your body starts to change, your reproductive organs mold, shift, and alter their form. You become the group's sperm producer. As a bluehead wrasse, you can have one body and one set of DNA, but multiple forms of reproductive biology across your lifetime.
]

What Fuentes fails to provide is any explanation, other than a vaguely group-functional need to "become the group's sperm producer," for why many wrasse and other teleost fish species are sequential or simultaneous hermaphrodites. He could have looked up the literature on this as easily as I did @avise_2009, and maybe he did, but if he did, he chose not to tell his readers.

And that's the pattern Fuentes follows in SIAS, which is long on descriptions of biological variation but very short on explanations, either those put forward in the scientific literature, or his own. Yet his Introduction promises, at least for humans, to deliver a "new narrative" about the biology of sex:

#quote(block: true)[
We have to understand what it means that everything about humans is a supercomplicated blend of biology and culture. We need to combine our knowledge of biology, sex, and the human experience into a new narrative. My goal in this book is to put forward this new narrative and show how the biology of sex actually works, what it does and does not tell us, and how we might incorporate this knowledge into our education, lives, and laws. (p.~3).
]

The tl;dr of my review is that he doesn't deliver a new narrative, nor any theoretical or conceptual insights or advances beyond "everything about humans is a supercomplicated blend of biology and culture."

= Preface: Theoretical background
<preface-theoretical-background>
To assess whether Fuentes identifies important biological limits of the sex binary as his title promises, I've written this somewhat lengthy preface to explain (1) why most evolutionary biologists define the sexes as anisogamy, (2) the current thinking about how anisogamy might have evolved, and (3) the implications for the evolution of anisogamous species. In other words, what would science be giving up if it ditched the binary sex concept? #emph[Caveat emptor: I am a consumer of this literature, not a producer, and I don't specialize in the evolution of sex or anisogamy];.

The Earth formed about 4.5 billion years ago @halliday2023, and, based on fossil evidence, life arose by 3.7 billion years ago, and perhaps much earlier @dodd2017. One genetics-based estimate inferred that the last universal common ancestor (LUCA) of all life was similar to modern-day prokaryotes and had evolved by 4.2 billion years ago @moody_2024. LUCA subsequently gave rise to the two top-level domains of life, the single-celled Archaea and Bacteria.

The evolution of sex commenced with the later evolution of eukaryotes, which have a more complex cellular structure. Eukaryotes were traditionally considered one of the three top-level domains of all life, but are now believed to have evolved from Archaea and then subsequently evolved a symbiosis with a bacterium (the origin of mitochondria), some 1.8-2.7 billion years ago @vosseberg_2024. See #ref(<fig-LECA>, supplement: [Figure]).

#figure([
#box(image("41586_2024_7677_Fig1_HTML.png"))
], caption: figure.caption(
position: bottom, 
[
The tree of life, showing the chimeric origin of the eukaryotic lineage and the period of eukaryogenesis. In this illustration, the eukaryotic lineage diverges slightly earlier from Asgard archaea compared with Alphaproteobacteria, as suggested recently. FECA, first eukaryotic common ancestor; FMCA, first mitochondrial common ancestor; LECA, last eukaryotic common ancestor. Figure and caption from @vosseberg_2024 (LUCA added for clarity).
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-LECA>


The eukaryotes include all multicellular plants, animals, and fungi, but most eukaryotic species are protists -- a diverse group of single-celled organisms @burki_2020.

Whereas Archaea and Bacteria reproduce clonally via cell division, almost all eukaryotes, including single-cell protists, reproduce sexually, at least some of the time. This apparently included the last eukaryotic common ancestor (LECA), meaning that sex is ancestral in the eukaryotes @speijer2015. Sexual reproduction is defined as the meiotic formation of haploid reproductive cells (gametes), where a gamete from one individual fuses with a gamete from (typically) another individual (the two parents) to form a diploid cell (the zygote, i.e., offspring).#footnote[There are complications, especially in plants and fungi. See these discussions of #link("https://en.wikipedia.org/wiki/Ploidy")[ploidy] and #link("https://en.wikipedia.org/wiki/Polyploidy")[polyploidy];.]

Note that #emph[two] parents play a central role in sex, as this is the ultimate reason there are two sexes in many species. Note also that sex is largely conserved through deep time -- perhaps a billion years or more -- and across millions of eukaryotic species, including humans. The theoretical scope of sex and its related concepts is vast.

There is no consensus on why sex evolved or has been maintained by selection across such a diverse range of species. Fuentes provides a brief account of one of the influential theories, but there are many others. This Wikipedia article provides an overview @Evolution_of_sexual_reproduction.

Sexual reproduction does not necessarily involve females and males. Before discussing the evolution of the two sexes, I need to introduce the concept of a "game" from game theory. A game comprises one or more players, their possible moves (also often termed options, choices, alternatives, or actions) at each of a number of stages, the information they have at each stage, and the payoffs for each player for each path through the game. Here is the extensive form of the ultimatum game, which will probably be familiar to most readers:

!\[The Ultimatum Game. Player 1 can offer any amount between \$0-10 to player 2. Player 2 can Accept the offer, in which case Player 1 keeps the remainder, or Reject it, in which case each player gets \$0. For more details, see #link("ultimatum-game.svg")[#cite(<Ultimatum_game>, form: "prose");]

The point that will be important shortly is that there is a key distinction between the players, and their options in the game. In general, the possible options for players can be continuous, as for player 1 in the ultimatum game, or discrete (binary, for example), as for player 2. A #emph[strategy] is a specification of the option a player will choose at each stage given the information she has at that stage. Strategies thus map information to actions. A key focus of game theory is to determine the optimal strategies for all players, i.e., the strategies that will maximize their payoffs given the strategies of the other players. (As an aside, the "solution" to the ultimatum game in #strong[?\@fig-ultimatum] is that player 1 should make the smallest positive offer (\$0.01), and player 2 should accept it because these strategies maximize the payoff for each player, given the strategy of the other player.)

In evolutionary game theory, one of the most popular frameworks for theorizing about organism traits, a game represents a situation faced by individuals in a population who can respond in different ways (their options), where the payoff is fitness (reproductive success). Mathematical analysis of the game aims to determine the strategies that will evolve by natural selection. The game concept can be applied not only to the evolution of behavioral traits but also to the evolution of physiological and morphological traits, such as gamete type and gamete size.

To reproduce sexually, a eukaryotic organism must produce a gamete -- a cell containing half the number of chromosomes of the somatic cell#footnote[There are complications, especially in plants and fungi. See these discussions of #link("https://en.wikipedia.org/wiki/Ploidy")[ploidy] and #link("https://en.wikipedia.org/wiki/Polyploidy")[polyploidy];.] -- which must then encounter and fuse with a gamete from (typically) another member of its species (the two parents) to form a zygote (the offspring).#footnote[In some organisms, e.g.~some ciliates and fungi, sex involves karyogamy (fusion of the nuclei) but not syngamy (fusion of gametes).]

In most species, gametes have "types", and only fuse with gametes of a different "type" (termed mating types).#footnote[In some species, gametes from the same individual can fuse to form a zygote, termed #emph[autogamy];.] In evolutionary game theory terms, producing gametes is a "game", the players are the potential parents, gamete "type" is a "choice" that each potential parent must make from a set of options, and the number of mating type options is determined by, e.g., the number of alleles at one or more mating type loci, with new options introduced by mutations. The number of mating types is typically two, but in some species ranges into the thousands. The evolution of mating types is still a matter of debate @constable_2018.

Gamete size is another critical "choice" in the gamete production game that each potential parent must make from a set of options. Gametes must survive long enough to encounter and fuse with another gamete, and the resulting zygote must be able to successfully survive and reproduce, all of which require sufficient provisions from the parents. Organisms could, in principle, choose to produce gametes that are all the same size (termed #emph[isogamy];), gametes that have a continuous spectrum of sizes, or gametes that have a discrete number of sizes, such as two: large and small (termed #emph[anisogamy];). A general pattern (albeit one with several exceptions) is that single-celled eukaryotes are isogamous with two mating types, whereas multicellular eukaryotes are anisogamous, producing either small gametes that only fuse with large ones, or large ones that only fuse with small ones. The latter is the biological definition of the sexes: small gametes are by definition male, and large ones female.

I want to draw out an implication that I think is not as widely recognized as it should be: in its most general form, which is meant to apply to the evolution of millions of multicellular species over deep time, the sex "binary" is #emph[not] a scheme to classify the potential parents (the "players"). Instead, it refers to the number of options for each potential parent in their gamete production game. In isogamous species, each potential parent produces gametes of the same size, i.e., each has only one gamete size option (no binary). Many species (mostly but not exclusively plants) are simultaneous hermaphrodites. In these species, each potential parent produces both large and small gametes. Hence, the potential parents cannot be classified as either male or female, but their gamete size options can be. In sequential hermaphrodites, such as the bluehead wrasse and many other fish species, potential parents produce one gamete size (typically the large one), and then some undergo substantial physiological changes to produce the other size. In yet other species, including humans, the gamete production game still has two options (large and small), but the "choice" is made early in development via a genetic or environmental randomization device (in humans, the 50% probability of inheriting an X or Y chromosome from the father), committing each potential parent to produce either large gametes or small ones, but not both (termed #emph[gonochorism];).

Importantly, gamete production is not the only game in town. Organisms have evolved to play many "games," such as territorial defense, predation, predator evasion, parasitism, parasite defense, and so forth. This means that although gamete production will have an important impact on the evolution of the phenotype (including the behavioral phenotype), many other factors will too.

Turning back to the two sexes and their evolution: given that organisms could, in principle, produce gametes with a continuum of sizes, a key question in evolutionary biology is why most multicellular organisms only have two options: large and small. One of the most influential evolutionary game theoretic accounts of the evolution of anisogamy assumes that producing gametes consumes limited resources, so sexually reproducing organisms have a choice between producing fewer large gametes or more small ones. #cite(<bulmer_2002>, form: "prose") explain:

#quote(block: true)[
Parker et al.~(1972) proposed that males and females originated through disruptive selection acting on an ancestral isogamous (i.e.~single sex) population. This arises from three very simple assumptions as follows.

#block[
#set enum(numbering: "(i)", start: 1)
+ In a primitive marine ancestor, individuals produce a range of gamete sizes, and fusion between pairs of gametes is at random in the sea.
+ Each parent has a fixed budget for reproduction, so that there is a size-number trade-off: the number of gametes produced is inversely proportional to their size.
+ The success (e.g.~viability) of the zygote increases with its size, or provisioning, which equals the sum of the sizes of the two fusing gametes.
]

In this model, the most frequent fusions would be between the smallest (S) gametes, but the resulting zygotes would experience low viability compared with zygotes arising from the fusion of large gametes (O). Thus, S-producers succeed by gaining most fusions with O gametes, and O-producers succeed because they generate zygotes with high viability, having more energy for development. Individuals producing intermediate-sized gametes (I), though originally commonest, decrease in frequency relative to S- and O-producers, resulting in a population consisting of proto-males and proto-females.
]

Note that in this scenario, the gamete size options were initially a continuum, and then, perhaps coincident with the evolution of multicellularity, evolved to be discrete (binary).

The evolution of anisogamy had many evolutionary consequences, not least of which was sexual selection. Here is one scenario from #cite(<parker2014>, form: "prose");, which he terms the "sexual cascade":

#figure([
#box(image("ParkerCascade.jpg"))
], caption: figure.caption(
position: bottom, 
[
The sexual cascade (succession of evolutionary events leading to Darwinian sexual selection) (pink boxes and arrows) showing main transitions and selective forces (white boxes and black arrows) and alternative stable states (blue boxes and arrows). For details, see #cite(<parker2014>, form: "prose");.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-cascade>


So far, I've discussed steps A and B in #ref(<fig-cascade>, supplement: [Figure]). Step C, known as the Fisher condition, is critical to understanding why the ratio of females to males is close to 1:1 (unity) in many species, including humans, but can deviate from 1:1 in others, a topic investigated in sex allocation theory @frank1990. Understanding and measuring sex ratios is key for the conservation of endangered species.

With the evolution of multicellularity in step D, organisms invest in specialized gonadal tissues, with consequent high levels of sperm competition in some species with, e.g., communal spawning, but reduced sperm competition others with noncommunal spawning. In step E, copulation evolves in some species, which reduces sperm competition but increases precopulatory (mating) competition and sexual selection, and the evolution of Darwinian sex roles. See #cite(<parker2014>, form: "prose") for details.

The take-homes from this preface are:

+ The sex binary (anisogamy) characterizes the evolution of millions of species across deep time.
+ The sex binary refers to the number of options in the gamete production "game", not necessarily to the number of parent "types". In gonochoric species, including humans, the number of options and number of parent types are the same (two), if by parent "types" we mean types that evolved by natural selection to produce one gamete size or the other.
+ The evolution of anisogamy has had many profound downstream consequences for the evolution of the biodiversity we see today, and is an essential concept in conservation efforts.
+ Abandoning the binary sex concept without a clearly superior alternative would kneecap our ability to understand an important fraction of life on earth, including #emph[Homo sapiens];.

= Back to my review…
<sec-review>
Remarkably for a book on the biology of sex written for a general audience, with the word "binary" in the title, Fuentes does not provide his readers with the Parker et al.~theory, or any other theory for the evolution of binary sex, i.e., anisogamy.#footnote[although he does cite #cite(<Lehtonen_2014>, form: "prose") in passing.] Instead, he summarizes historian Thomas Laqueur's #cite(<laqueur1992>, form: "year") influential account of Western ideas about the sexes, the one-sex, two-sex model:

#quote(block: true)[
The idea of biologically distinct sexes only became common in the eighteenth century. From the early Greek ideas about sex of Aristotle and Hippocrates and the Roman anatomical ideas of Galen through the Renaissance, and into the start of the eighteenth century, science and the medical world did not consider females and males as two separate kinds of biologies or beings. Rather, they were seen as hierarchically ranked (male above female) versions of the human form. This was termed the "one sex model."8 But from the eighteenth into the nineteenth century, the belief in a "two sex" model, with males and females reflecting different biologies, emerged, but it had no specific definitional focus. It is from this newer two-sex worldview that the hypothesis of differences in gamete size as the key to female and male distinction emerged. (SIAS, p.~9).
]

Laqueur's model, detailed in his book #emph[Making Sex: Body and Gender from the Greeks to Freud];, is based in part on the claim of Galen, a physician in ancient Rome, that "all the parts, then, that men have, women have too, the difference between them lying in only one thing… , namely, that in women the parts are within \[the body\], whereas in men they are outside…." @Park_2023[p 157].

There are many criticisms of Laqueur's model. For example, the #emph[Myth of the "One-Sex" Body] by the Harvard historian of science, Katharine Park, struck me (admittedly not an historian) as a convincing take-down of Laqueur's one-sex model @Park_2023. This particular criticism, one of many, stood out for me:

#quote(block: true)[
While immersed in Galenic medical theory, \[Al-Raz̄ı̄a and al-Majus̄ı̄w\] were closely attuned to the realities of their patients' bodies, including differences between the illnesses suffered by their male and female clients, alongside which qualified anatomical similarities between the male and female genitals paled in importance. Because men neither menstruated, miscarried, nor gave birth, the main pathological conditions related to these functions were unique to women, including hypermenorrhea and amenorrhea, obstetrical fistula, vaginal and perineal tears, and uterine prolapse.34 While it was theoretically possible to shoehorn these conditions into a system shaped by strict parallels between the male and female genitals, the stark differences between the illnesses associated with each set of structures muted their similarities. It is hard to imagine any context in which a homology between the scrotum and the uterus could prove helpful for the treatment of either organ. @Park_2023[p.~161]
]

Although Park credits Laqueur for opening up an important topic in the history of early medicine, her conclusion doesn't pull any punches:

#quote(block: true)[
Laqueur's account of the "one-sex" body is an attractive fiction, as well as a good example of the inadvisability of making sweeping statements about textual traditions in languages one cannot read. @Park_2023[p 176]
]

The long Western history of pre-Darwinian ideas about the sexes is obviously important and complex, but in the end Fuentes does not dispute the role of anisogamy. In fact, for much of his book instead of using the terms "female" and "male" he uses "large-gamete producers" and "small-gamete producers" (later he switches to "3G females" and "3G males", where the "3G" refers to genes, gonads, and genitals).

Fuentes next discusses the Darwin-Bateman paradigm:

#quote(block: true)[
Bateman's \[1948\] argument (cheap sperm and costly ova make males and females very different organisms) became baseline theory for evolutionary biology around 1966 and centralized in the world of assumed "biological fact" by another biologist, R. L. Trivers, in 1972.13 Trivers connected anisogamy to parental care via a simple mathematical equation. Bateman's notions of sex differences entered near universal "truth" in biology with the work of E. O. Wilson in 1975 and G. A. Parker in 1979.14 As it turns out, Bateman was mostly wrong. (p.~10).
]

Was Bateman mostly wrong? The modern version of Bateman's argument is known as a Bateman gradient, which is the slope of reproductive success on the number of mates. The Bateman gradient is a key component of the strength of sexual selection and is typically expected to be steeper in males than females. Why? Bateman linked the purported sex difference in the gradient to anisogamy: males produce many small gametes, and females fewer large ones. To give an extreme example: if, due to resource constraints, each female produced one large gamete in her lifetime, and each male produced 10 small ones, then a female could only have one offspring if she can acquire at least one mate, whereas a male could potentially have 10 offspring if he were able to acquire 10 mates. In addition, if the sex ratio is 1:1, then if some males have more than 1 mate, on average, others have less, on average, which would increase the variance in male relative to female reproductive success (e.g., some males have 10 offspring and others 0, whereas all females have 1 offspring), sexually selecting for whatever traits increased males' access to mates.

#cite(<Lehtonen_2022>, form: "prose") shows that this verbal argument holds up to mathematical scrutiny:

#quote(block: true)[
The first two \[mathematical\] models confirm that gamete number asymmetry alone creates asymmetry in Bateman gradients as Bateman's assertion suggests, while the third model demonstrates that if fertilisation is efficient, this outcome remains valid despite the asymmetric roles of female as gamete recipient and male as gamete donor. However, inefficient fertilisation can push Bateman gradients towards equality under external fertilisation and reverse them under internal fertilisation. Overall, the results show that despite interesting exceptions, Bateman's assertion is correct under relatively general conditions.
]

Empirically, Bateman's original 1948 experiment with fruit flies had problems, as Fuentes points out, and a previous replication attempt failed to support Bateman's hypothesis. However, a recent replication of Bateman's experiment, modified to address the original problems @davies2023, supported Bateman:

#quote(block: true)[
(a) males had significantly more variation in number of mates compared with females and (b) males had significantly more individual variation in total number of offspring. We also find a significantly steeper Bateman gradient for males compared to females, suggesting that sexual selection is operating more intensely in males. However, female remating was limited, providing the opportunity for future study to further explore female reproductive success in correlation with higher levels of remating.
]

Moreover, in a meta-analysis of 72 studies of 66 species titled "Darwinian sex roles confirmed across the animal kingdom", #cite(<Janicke_2016>, form: "prose") found that:

#quote(block: true)[
across the animal kingdom, sexual selection, as captured by standard Bateman metrics, is indeed stronger in males than in females and that it is evolutionarily tied to sex biases in parental care and sexual dimorphism. Our findings provide the first comprehensive evidence that Darwin's concept of conventional sex roles is accurate and refute recent criticism of sexual selection theory.
]

Fuentes cites this study, curiously not in his discussion of Bateman but in a subsequent chapter in support of this statement:

#quote(block: true)[
Large- and small-gamete producers can and do experience different evolutionary pressures resulting in some sexual selection for sex-biology "roles." But any one pattern, even if it's very common, is seldom ubiquitous, and the actual biological outcomes---what the "roles" and bodies look like and do---vary substantially across different species and lineages. (SIAS, p.~23).
]

The above quote illustrates Fuentes' rhetorical tactic throughout SIAS: a begrudging #emph[yes, there are some evolved sex differences in traits X, Y, and Z, but it varies!]

= Chapter 2 Animal Sex Biology: Mixing it up
<chapter-2-animal-sex-biology-mixing-it-up>
In his chapter on animal sex biology, Fuentes briefly describes the hymenopteran haplodiploid system, insect sex determination systems, hermaphroditic and parthenogenic earthworms, #emph[\C. elegans] sex determination, sequentially hermaphroditic teleost fishes, female mimics, and sex changes in a variety of other fish species, yet despite a rich theoretical and empirical literature on all of these, he provides no explanations for any of them. But he does (finally!) provide one short possible explanation for gobies. Here it is in its entirety:

#quote(block: true)[
The ability to shift sex biology flexibly by all individuals in a population reduces the costs and risks incurred by moving between the coral clusters where the gobies like to live. (SIAS, p.~28).
]

Turning to reptiles, birds, and mammals, species with internal fertilization, Fuentes informs us:

#quote(block: true)[
It is in these birds, reptiles, and especially mammals, where one sees, on average, greater physiological differences between the sex biologies of large- and small-gamete producers. However, what those patterns are across and within species, what is "typical" for any given reproductive physiology, and how much overlap exists between the bodies and behavior of large- and small-gamete producers within a given species vary quite a lot. (p.~28).
]

Fuentes provides brief descriptions of temperature-dependent sex determination, with no explanations, and sexual body dimorphism, where he briefly explains in one sentence that the latter might be due to sexual selection and environmental constraints on body size. He devotes a paragraph to the interesting genetic system of parthenogenic whiptail lizards, but with no explanations, and notes that most reptiles provide little parental investment beyond provisioning eggs; again, no explanations.

For variation in biparental care in birds, Fuentes suggests a few factors that might explain it in one sentence, but without providing any actual explanations:

#quote(block: true)[
While most birds have biparental care of young, the details of that caregiving vary widely. How large- and small-gamete producers contribute to such care is related to differences in social systems, mating systems, environmental context, and bodies.22 (p.~30)
]

Curious readers will have to look to reference 22. The same goes for sexual dimorphism:

#quote(block: true)[
In many bird species, there is sexual dimorphism (differences in the shapes, sizes, and/or colors of bodies) between large- and small-gamete producers, but what form that takes is not consistent or in a single pattern across all species. Body-size differences between large- and small-gamete producers often relate to aspects of social and mating systems, to the patterns of parental care, and to aspects of the specific ecologies of that species. (pp.~30-31)
]

Relate how? Fuentes provides no clue.

The mammal examples are the spotted hyena pseudopenis, with no explanations except that it is "mediated by a very complex interplay between a range of multiple hormones" (p.~33); and monogamous primates and naked mole rats, with no explanations.

For Fuentes, it seems, biology is merely a cabinet of curiosities. See #ref(<fig-curiosities>, supplement: [Figure]).

#figure([
#box(image("RitrattoMuseoFerranteImperato.jpg"))
], caption: figure.caption(
position: bottom, 
[
Fold-out engraving from Ferrante Imperato's Dell'Historia Naturale (Naples 1599), the earliest illustration of a natural history cabinet. #link("https://en.wikipedia.org/wiki/Cabinet_of_curiosities#To_c._1600")[Wikipedia]
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-curiosities>


To be clear, existing explanations for most of Fuentes' examples are still debated, sometimes heavily, yet the point of studying biological variation is to propose and test explanations.#footnote[For example, #cite(<muller2002>, form: "prose") proposed: "The genitals of spotted hyena females are not simply masculinized, but exhibit a detailed physical resemblance to the male genitalia. In the absence of satisfactory alternative explanations, we propose that selection may have favored sexual mimicry in females because they are more likely than males to be targets of aggression from other females. Male‐like camouflage could theoretically be protective in three contexts: neonate sibling aggression, infanticide by conspecific females, and interclan territoriality."] Fuentes' "new narrative" has provided nothing except well-known examples of biological variation minus possible explanations for that variation that have been put forward by others, nor does he provide any explanations of his own.

= Chapters 3-7: Humans are Messy \[etc.\]
<chapters-3-7-humans-are-messy-etc.>
Fuentes' central theme is that humans are "biocultural":

#quote(block: true)[
Although we are biological organisms, the totality of the human experience cannot be reduced to either specific innate (biological) or external (environmental/cultural) influences. It is a synthesis of both: humans are biocultural. (p.~41)
]

For Fuentes, a biocultural account of gender and sex requires deliberately conflating these distinct concepts as "gender/sex" to "reflect the intertwined biocultural reality of bodies and experiences more accurately" (p.~45). Biological anthropologists Phil Walker and Della Cook #cite(<walker1998>, form: "year", supplement: [p 255]) critique this approach better than I could in an article titled "Gender and Sex: Vive la Difference":

#quote(block: true)[
A failure to make the distinction between gender and sex is analytically incapacitating in a field such as physical anthropology, whose strength lies in the integration of biological and culturual information.
]

The importance of distinguishing between gender and sex is nicely illustrated by a case highlighted by Fuentes in which the biological sex and elaborate burial of a Copper Age skeleton did not align with contemporary conceptions of gender roles.

Fuentes then presents a fairly standard account of human evolution since our divergence from chimpanzees in which he rightly emphasizes the importance of cooperative child rearing (alloparenting), evidence for which comes from work with contemporary hunter-gatherer and other groups that also typically exhibit marked sexual divisions of labor. Although Fuentes briefly mentions sexual divisions of labor, he downplays it:

#quote(block: true)[
In some contemporary human societies, men (the gender) do hunt large game more than women (the gender), and in many societies, mothers, grandmothers, and sisters are the main individuals charged with childcare. However, assumptions about such role differentiation and its patterns in the human past, especially in the context of hunting and caretaking, are challenged via a range of archeological, fossil, and recent ethnographic analyses. (p.~70).
]

One of the papers he cites that supposedly challenges a sexual division of labor in the human past is #cite(<kuhn2006>, form: "prose");, who argue that, due to its fitness advantages, it was precisely the sexual division of labor in human populations expanding out of tropical Africa that might explain why they replaced Neanderthals. Another paper Fuentes cites here, #cite(<Anderson_2023>, form: "prose");, had fatal flaws in its data collection and analysis @venkataraman2024.

Fuentes also doesn't cite the hugely influential work of Hillard Kaplan and colleagues that emphasizes the importance of a sexual division of labor in human evolution @kaplan2000@kaplan2009. Their basic idea is that, whereas ape mothers provide all the calories for their infants, who are able to provision themselves on plant foods in just a few years, human mothers invest heavily in child care and foraging for plant foods and small game that are reliable but lower return, and men forage for high risk, high return game, producing a surplus, and the combined calories provision multiple offspring who take the better part of two decades to acquire these complex foraging skills. In short, it was the sexual division of labor that underpinned the "cultural" part of our biocultural species.

Here is my plot of data from a recent #emph[Science] paper by #cite(<kraft2021>, form: "prose");, also not cited by Fuentes, that presents evidence for the foregoing scenario. Note that women generally provide enough calories to support themselves (with exceptions) and men produce a surplus (with exceptions). See #ref(<fig-energy>, supplement: [Figure]).

#figure([
#box(image("plot_energy.svg"))
], caption: figure.caption(
position: bottom, 
[
Daily per capita adult energy production by females and males across ten contemporary hunter-gatherer populations. Boxplots in the top panel indicate the distributions of female, male, and total production depicted in the bottom panel. Total (pair bond): average production of one adult female plus one adult male. Grey bars indicate the range of average female (lower) to male (higher) total energy expenditure (TEE) for chimpanzees (left) and adult Hadza (right) measured using doubly labeled water. Dotted line indicates total TEE for one Hadza female + one male. Chimpanzee energy production is assumed to equal total energy expenditure (i.e., no surplus production other than for lactation). Data from #cite(<kraft2021>, form: "prose") and #cite(<pontzer2015>, form: "prose");.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-energy>


And here are data supporting the importance of allocare in hunter-gatherers and other traditional societies, but which also show that mothers do by far the most infant care, usually followed by older siblings. See #ref(<fig-allocare>, supplement: [Figure]) and #ref(<fig-allocare2>, supplement: [Figure])

#figure([
#box(image("1-s2.0-S003193841830115X-gr1_lrg.jpg"))
], caption: figure.caption(
position: bottom, 
[
Percent of direct care received by an infant that is provided by mothers and allo-caretakers. Siblings indicates total care provided by one or more siblings of the infant. Figure from #cite(<Kramer_2018>, form: "prose")
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-allocare>


#figure([
#box(image("index_files/figure-typst/fig-allocare2-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Variation in allomaternal infant care in 141 societies. Colors represent the main sources of subsistence. Data from #cite(<Martin_2020>, form: "prose");.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-allocare2>


It's not that Fuentes gets anything horribly wrong in his chapters on humans, past and present, it's that he fails to provide any, and I mean #emph[any];, alternative to the standard view that binary sex is but one contributor to variation in phenotypes.

To give an example, here is Fuentes discussing the sex difference in human height:

#quote(block: true)[
Height in humans is not a sex binary and not a true dimorphism. Height is one morph (a measurable shape) with a range of variation that can be divided into overlapping clusters composed of 3G males and 3G females. But it does not automatically have to be divided that way. Height distribution can also be sorted by age, by people from different latitudes and dietary practices, by athletes versus non-athletes, and by a range of other variables depending on what questions you are asking. There are not two forms of human, a tall and a short version; rather, there is a range of variation with some patterns in that variation. (p.~74).
]

The fact that binary sex only explains some of the variation in height and other traits in humans and other species, but not all, is entirely expected. This no reason to abandon the binary sex concept -- recall from my preface that species have been playing many "games" over the course of their evolution, all of which have had an impact on the evolution of their phenotypes. Accounting for the impact of multiple factors on some trait, including factors like nutrition that operate during development, is bog-standard life science.

Fuentes does acknowledge that there are some dramatic evolved sex differences, such as the presence or absence of a uterus. He also acknowledges sex differences in body composition, such as in body fat and upper body strength:

#quote(block: true)[
The pattern of fat deposition is the same for 3G sexes until puberty, when 3G females usually begin to increase total fat mass, eventually developing about 10 percent more total fat, on average, than a 3G male of the same height and weight. (p.~87).

\[It\] is not totally clear what role size difference as a category is playing relative to 3G sex as category of comparison…But some patterns of between 3G category measured strength still exist, to some degree, when comparisons are made between same height and weight 3G-female and 3G-male individuals. (pp.~81-82)
]

Sex differences in body composition (amounts and distributions of fat and muscle mass) provide a strong clue that women and men were engaged in different activities over the course of human evolution (on average!). Fuentes draws on NHANES data, so I will too. Regarding percent body fat, there is an increase in females after puberty, as Fuentes notes, and a decrease in males. See #ref(<fig-fat>, supplement: [Figure]).

#figure([
#box(image("index_files/figure-typst/fig-fat-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Total Percent Fat vs.~age in females (red) and males (purple), measured with Whole Body Dual-Energy X-ray Absorptiometry. Data from NHANES 2011-2014, a representative sample of the U.S. civilian noninstitutionalized population. Transparency represents the sampling weight for that individual. N = 9577.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-fat>


Regarding upper body strength, there is a bit more than "some degree" of sex difference, even when accounting for sex differences in height, which I've done here by plotting the two against each other (and it's possible that sex differences in height and upper body strength evolved together). See #ref(<fig-grip>, supplement: [Figure]).

#figure([
#box(image("index_files/figure-typst/fig-grip-1.svg"))
], caption: figure.caption(
position: bottom, 
[
#strong[Top];: Grip strength vs.~age in females (red) and males (purple). #strong[Bottom];: Grip strength vs.~height in adult females and males, ages 18-60. Data from NHANES 2011-2014, a representative sample of the U.S. civilian noninstitutionalized population. Grip strength is the sum of both hands. Each dot is one individual. Transparency represents the sampling weight for that individual. N = 7563.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-grip>


The proximate cause of the sex difference in strength is likely the pubertal surge in testosterone levels in males relative to females (but in adults, variation in strength is mostly unrelated to variation in testosterone). See #ref(<fig-testosterone>, supplement: [Figure]).

#figure([
#box(image("index_files/figure-typst/fig-testosterone-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Total serum testosterone vs.~age in individuals age 6-80+. Females (red) and males (purple). Data from NHANES 2011-2014, a representative sample of the U.S. civilian noninstitutionalized population. Each dot is one individual; transparency represents the sampling weight for that individual. To better display small values, data clipped at T \< 1000 (top) or log 10 transformed (bottom). N = 14385.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-testosterone>


Readers can judge for themselves if the evidence warrants a possible evolutionary explanation for the sex differences in body composition, but if so, what is it? Lassek and Gaulin #cite(<lassek2009>, form: "year");#cite(<lassek2022>, form: "year") argue that these are explained by sexual selection on males and natural selection on females, but Fuentes dismisses their work:

#quote(block: true)[
\[S\]ome researchers assert that "sex differences in muscle and strength are largely the result of sexual selection for male-male competitive traits, whereas females have increased body fat (in place of muscle) due to natural selection for maternal investment capacity in the context of our unusually large brains."5….Chapters 4 and 5 outlined why these assumptions don't map onto what we know about human evolution and how they oversimplify descriptions of the existing variation in human biology.

…

So, as we see, there are scholars who argue that human 3G-category bodies and minds are deeply and evolutionarily different and that this "biological reality" places human men and women in direct conflict. And there is a large swath of the public that believes this as well. However, scientific data and analyses demonstrate that this sex-conflict position is not true. (SIAS, p.~110-111).
]

Lassek and Gaulin actually argue that men were in physical conflict with other men (as Fuentes notes initially), not with women. #cite(<smith2025>, form: "prose") replicated most of the results of #cite(<lassek2009>, form: "prose") regarding the association of male upper body strength with mating success, supporting their sexual selection hypothesis, but with some twists: in addition to male-male physical competition, a cooperative sexual division of labor and female choice might also have played a role.

= Straw men
<straw-men>
Fuentes is rarely wrong when he downplays or dismisses evidence of evolved sex differences, either across species or in humans, because the versions he downplays or dismisses are classic straw men. Here are several examples, where I've bolded his hedges and caveats:

- "\[T\]here is rarely a #strong[perfect one-to-one] correlation between patterns of biological variation and peoples' lived experiences of their bodies through gender." (p.~44)
- "Gender, as a cultural experience, is not consistent or #strong[uniform];." (p.~44)
- "Gender and biology have a complex relationship---#strong[not a 1:1 correlation];." (p.~45).
- "As we've already discussed, this assumption based on gamete size and related investment is #strong[not wholly] supported, and the original model of what anisogamy implies is #strong[not universally] accurate." (p.~51).
- "Sex biology in the ape experience is variable and is #strong[not universally] tied to one kind of relationship between bodies, behavior, what type of gametes are produced…." (p.~56).
- "In the nineteenth century, it was clear to scientists studying human biology that a range of external genital morphology was possible, and there was no way to sort it into two #strong[totally distinct] categories." (p.~84). \
- "…while there are gendered patterns of behavior that matter, there is no evidence for an evolved pattern of specific 3G-male #strong[hyperaggression] as a key adaption in humans." (p.~120)

Fuentes has promised the reader a "new narrative" about the biology of sex, which suggests new theories and new explanations for biological variation that surpass existing ones, yet the book is mostly brief examples of biological variation in humans and other animals that are only very rarely accompanied by explanations by Fuentes or anyone else. Readers who are familiar with the examples will know that some have widely accepted explanations, some have hotly debated explanations, and some have highly speculative explanations. But unless they track down the cited references, the general target audience for this book won't have an inkling that any explanations for this variation exist at all.

For both humans and other species, Fuentes has it backwards: biological variation doesn't undermine the anisogamy concept; instead, anisogamy has been one of the most powerful concepts for #emph[explaining] that variation.

= Is the sex binary "harmful"?
<is-the-sex-binary-harmful>
In his book #emph[Rocks of Ages];, Stephen J. Gould #cite(<gould2011>, form: "year", supplement: [.]) argued that Science and Religion were two non-overlapping magisteria:

#quote(block: true)[
Science tries to document the factual character of the natural world, and to develop theories that coordinate and explain these facts. Religion, on the other hand, operates in the equally important, but utterly different, realm of human purposes, meanings, and values--subjects that the factual domain of science might illuminate, but can never resolve. (Rocks of Ages, p.~4)
]

By "religion" Gould meant the framework of morals and values we agree to live by, which historically was organized religion but now needn't be.#footnote[As Gould elaborates in #emph[Rocks of Ages];: "These questions address moral issues about the value and meaning of life, both in human form and more widely construed. Their fruitful discussion must proceed under a different magisterium, far older than science (at least as a formalized inquiry), and dedicated to a quest for consensus, or at least a clarification of assumptions and criteria, about ethical “ought," rather than a search for any factual "is" about the material construction of the natural world. This magisterium of ethical discussion and search for meaning includes several disciplines traditionally grouped under the humanities---much of philosophy, and part of literature and history, for example. But human societies have usually centered the discourse of this magisterium upon an institution called "religion" (and manifesting, under this single name, an astonishing diversity of approaches, including all possible beliefs about the nature, or existence for that matter, of divine power; and all possible attitudes to freedom of discussion vs.~obedience to unchangeable texts or doctrines).”] He went on:

#quote(block: true)[
If religion can no longer dictate the nature of factual conclusions residing properly within the magisterium of science, then scientists cannot claim higher insight into moral truth from any superior knowledge of the world's empirical constitution. (Rocks of Ages, p.~11)
]

According to Gould, though Darwin provided the principles needed to explain the diversity of life, he clearly recognized that those principles were not moral instructions.

In the last part of SIAS, Fuentes weighs in on several topics that fall squarely in Gould's "religious" or moral magisterium, such as laws restricting rights of LGBTQ folks to raise families, regulating who gets to compete in sporting events, and who gets to use which public restrooms, and Fuentes lays the blame for bad outcomes on the sex binary:

#quote(block: true)[
\[A\]llegiance to the binary view in industry, government, education, and by the public fosters ignorance, harm, and suffering. (p.~125)
]

Fuentes is right that these are important issues, but science can't resolve them. Science can provide valuable information to policy makers and the public, and Fuentes does: there is no evidence for a "gay gene", for example, and children raised in LGBTQ families fare just as well as those raised in non-LGBTQ families. But grounding a moral framework in science is a recipe for disaster. If SIAS is science, then it might be wrong, just as explanations based on anisogamy might be wrong. If the rights of, e.g., LBGTQ folks were grounded in SIAS and it turns out to be wrong, their rights could collapse (and the same goes for grounding rights in anisogamy).

#emph[Human rights don't depend on which scientific views turn out to be correct.]

Despite my criticisms, I suspect Fuentes and I agree more than we disagree. First, we agree that scientific theories grounded in binary sex are subject to criticism (and the criticisms have improved the theories). For example, #cite(<de_Vries_2023>, form: "prose") acknowledge "the majority of causal models in sexual selection theory do make sex-specific assumptions beyond the definition of the sexes, and do not clearly define the sexes," with common assumptions being that only males display and only females choose, and/or that female fecundity doesn't depend on male abundance but male fecundity does depend on female abundance.

I suspect we also agree that the physiological and psychological similarities between human females and males far outweigh the differences, that sex is but one of many factors that impact phenotypes, that for many traits sex doesn't account for most individual variation or even much at all, that across species the impact of sex on physical and behavioral traits varies a lot, and that much about the diversity of life, human and nonhuman, remains to be explained.

 
  
#set bibliography(style: "human-nature.csl") 


#bibliography("references.bib")

