# What is Evam-tools?
***
```Evam-tools``` is a package that allows to infeer cancer evolutionary pathways  starting from [_cross sectional data **(CSD)**_](#helpcsd) base on the output of [_cancer progression models  **(CPMs)**_](#cpms).

This web interface provides a user-friendly interactive version of the package. 
Here you can define complex scenarios with few click and check the predictions of several softwares.  

* In the ```Input``` tab you can define data sets to run ```Evam-tools```.
* In the ```Results``` tab you can see the output of [_cancer progression models  **(CPMs)**_](#cpms). 
* In the ```Examples``` tab you can see some pre-defined data sets that you can run or modify. 

<center>
<img src="evamtools.png" width=850/>
</center>

```Evam-tools``` is is also available as an R command package.
Link to the package. 


# What is _cross-sectional data_?<a id="helpcsd"></a> 
***

_CSD_ represents a collection of screened patients. 
For each of those patients, the state of several events has been checked. 
For events we can refeer to single point mutations, insertions, deletions or any other genetic modification.

In summary, _CSD_ is a binary matrix, filled either with _0_ if an event is not observed in a patient, or with _1_ if it is observed.

# How to use this web interface? <a id="input"></a>
***
The first step is to **define an scenario**.
You can do it by going to the ```User Input``` tab in the navigation bar at the top.

This web interface allows to define _CSD_ in three different ways:

* _By directly defining genotypes frequencies_ : you directly define what mutations are observed in how many patients.
* _By deriving genotype frequencies from a **direct acyclic graph (DAG)**_ : here you define 
dependency relationships between genes.
* _By deriving genotype frequencies from a **transition rate matrix**_ : the transition rate matrix reflects how every gene affects each other, by making them more likely to mutate (positive theta) or less likely (negative theta).

You can also use precomputed _CSD_ in the ```Examples``` tab and modify them.

Once you have created an scenario or selected one, you can hit the ```Run evamtools!``` button.
This will run several [_cancer progression models  **(CPMs)**_](#cpms) and will display their [results](#helpresults). 

You can also increase or decrease the number of genes, or rename genes.

# How to build an scenario of cancer evolutions that makes some sense? A simple example <a id="examples"></a>

Image a simple scenarios where only study 3 genes (A, B ,and C). 

We sample 12 patients and we observe the following:
<center>

|            | A | B | C |
|:----------:|:-:|:-:|:-:|
| Patient  1 | 1 | 0 | 0 |
| Patient  2 | 1 | 0 | 0 |
| Patient  3 | 1 | 0 | 0 |
| Patient  4 | 1 | 0 | 0 |
| Patient  5 | 1 | 1 | 0 |
| Patient  6 | 1 | 1 | 0 |
| Patient  7 | 1 | 1 | 0 |
| Patient  8 | 1 | 1 | 0 |
| Patient  9 | 1 | 1 | 1 |
| Patient  10 | 1 | 1 | 1 |
| Patient  11 | 1 | 1 | 1 |
| Patient  12 | 1 | 1 | 1 |

</center>

Gene _A_ appers alone. Gene _B_ **always** appears when gene _A_ is mutated.
Hence, we can infeer thats the likelihood gene _B_ mutating is increased if gene _A_ is also mutated.
The same happens for gene _C_, it is only mutated if both genes _A_ and _B_ are also mutated.

In this example, we sample 16 patients and we observe the following:

<center>

|            | A | B | C |
|------------|---|---|---|
| Patient  1 | 1 | 0 | 0 |
| Patient  2 | 1 | 0 | 0 |
| Patient  3 | 1 | 0 | 0 |
| Patient  4 | 1 | 0 | 0 |
| Patient  5 | 0 | 1 | 0 |
| Patient  6 | 0 | 1 | 0 |
| Patient  7 | 0 | 1 | 0 |
| Patient  8 | 0 | 1 | 0 |
| Patient  9 | 1 | 1 | 0 |
| Patient  10 | 1 | 1 | 0 |
| Patient  11 | 1 | 1 | 0 |
| Patient  12 | 1 | 1 | 0 |
| Patient  13 | 1 | 1 | 1 |
| Patient  14 | 1 | 1 | 1 |
| Patient  15 | 1 | 1 | 1 |
| Patient  16 | 1 | 1 | 1 |

</center>

Genes _A_ and _B_ appears mutated either in combination or isolated. 
In principle, there is not a dependency relationship between those two

However, gene _C_ only appears mutated if _A_ and _B_ are also mutated.
We can infeer dependecy relationship:

>booth _A_ **AND** _B_ has to be mutated for mutation in gene _C_ to appear.

# How to interpret the ```Results```?<a id="helpresults"></a>
***

The results sections looks like this:

Add screnshot

1. *Plotting the model*: here you can see either the **DAG** of each CPM showing the infered dependency relationships or the **transition rate matrix** in the case some [_CPMs_](#cpms).
1. *Plotting the sampling*: this plot is a bit more complex. It represents the flow that we have sampled using the output of its [_CPM_](#cpms). It highlights the most relevant genotypes and transitions between genotypes. It is created by making random samples using the parameters from the [_CPM_](#cpms) and counting the transitions observed and counting the transitions observed (defining edge width) and the genotype frequency (node size).  

<center>
<img src="transitions_dbxor.png" width=850>
</center>

3. *Tabular data*: represents the raw values computed from the model or extracted from the samples. This includes: 
  
  * *Transition rates*: rates of an exponential that model the trnasition from one genotype to another. This option is not available for OT or DBN.
  * *Genotype transitions counts*: times a transition from genotype A to genotype B has been observed when sampling. This option is not available for OT or DBN.
  * *Genotype frequencies*: frequency of each genotype. This option is not available for OT or DBN.
  * *Transition probabilities*: conditional probabilities of transitions to a genotypes given a previous one.
  * *Lambdas/probabilities*: parameters of each model. This option is not available for MHN. 
  * *Time-discretized transition matrix*:  the time-discretized version of the transition probability matrix. This option is not available for OT or DBN.


# What is a cancer progression model (CPM)?<a id="cpms"></a>
***

Cancer progression models (CPMs) use cross-sectional data to infer probabilistic relationships between mutational events that lead to the disease. 

# What CPMs are included in ```Evam-tools```?<a id="cpms"></a>
***

*  **Oncogenetic Tress (OT):** this is the simplest graphical model. Restrictions are represented as a tree. Hence, a parent node can have many children, but children have a single parent.
*  **Conjuntive Bayesian Networks (CBN):** this model generalizes the tree-based restricion of OT to a direct acyclic graph (DAG). A DAG allows to include multiple parents. In CBN, when a node depends of many parent events, all of the them have to be present for the children to appear. In that sense, CBN models this relationships as conjuntive, in other words, it models the AND relationship.
*  **Disjuntive Bayesian Networks (DBN):** models multiple parent with the OR relationship.
*  **Monte Carlo CBN (MCCBN):** this is an implementation of CBN using Monte Carlo expectationi-maximization algorithm to work with a large number of mutations.
*  **Progression Models of Cancer Evolution (PMCE):** is also a graphical model which main features it the aumatic detection of logical formulas AND, OR and XOR.
*  **Mutual Hazard networks (MHN):** in this model dpeendencies are not deterministic. In that sense, we do not see a direct dependence relationship, i.e. mutation B depends on mutation A. Conversely, an envent is influenced by all others. So, one event can make other more like (the presence of A promotes B) and also inhibiting it (the presence of A inhibits B). Hence, MHN includes multiple dependencies and is not limited to DAG schemes. The main parameters is a theta matrix that represents how one event influences other.

for styling

1. increase font size
2. blue for titles and for rule
3. table: make it wider, with more space
4. padding top and botton in image

make this the landing page