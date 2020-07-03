### General features of the models:
* The set of possible utterances is restricted to: "That dog is {big,small}", "That SUB_N is {big,small}", "That's a {big, small} dog", "That's a {big, small} SUB_N"
* The subject is assumed to (rather) contribute to reference, the predicate to predication.
* The representation of adjective meaning relies on threshold semantics
* The representation of comparison class influence relies on the previous comparison class model by [Tessler et al., (2017)](http://www.problang.org/chapters/05-vagueness.html)

### Models
* `separate_L0s_inContext_wL1.wwpl`:
  * to match experimental set-up, the target referent appears in a either basic-level or subordinate context, known to L1 (consisting of explicitly represented individuals)
  * L1 reasons about the intended referent and the comparison class, where the referent is some member of the context (i.e. L1 has perfect access to its size, and just doesn't know its identity)
  * additionally, L1 infers whether reference was established or not (represented by a boolen variable `establishRef`)
  * there are two separate L0s (and correspondingly, two meaning functions): one for reference (i.e., subject interpretation), and one for predication interpretation (i.e. predicate interpretation)
  * the value of the variable `establishRef` influences the state space of reference-L0: if establishRef = T, L0 already knows which state the utterance is referring to, and hence any utterance is considered as true; otherwise, the context is considered as the potential state space  
  * S1 maximises the summed utility of reference and predication
  * by making the S1 condition on the comparison classes, the use of felicitous comparison classes is ensured (i.e., when talking about a Great Dane, the speaker won't say "big dalmatian") (s.t. L1 predictions match expectations)
* `separateL0s_wQUD_wL1_wExptContext.wppl`:
  * similarly to the model above, there are two separate L0s for each informational goal, respectively
  * however, the context is supposed to match the experimental set-up more closely: a target of a subordinate category known to L1 appears in either a basic-level or a subordinate context; but the size of the target is unknown to L1
  * L1 makes an inference over the size and comparison class distribution; additionally, she makes an inference over the intended informational goal (reference vs. predication), represented by the variable `QUD`
  * the variable QUD determines the informational goal maximized by S1: S1 chooses the maximally informative subject for QUD=reference, and the maximally informative predicate for QUD=predication by sampling the position of the N (subject or predicate).  
* `jointL0_inExptContext_wL1.wppl`:
  * the representation of the set-up is supposed to match the experimental set-up: a target of a known subordinate category appears in either a basic-level or a subordinate context
  * joint L0 evaluates an utterance given a known comparison class and context (via one joint meaning function for subject && predicate)
  * states are sampled by L0: the referent is sampled from the union of target & context; the size from the distribution according to comparison class inferred by L1 / from predicate-N of the sentence
  * S1 adjusts the chosen utterances according to category of the target and the intended comparison class (for predicate-N); the referential utility is computed implicitly via conditioning on samples from L0' matching the intended referent  
  * L1 assumed to know the subordinate category of the target referent and the context; L1 infers the distributions over size and the comparison class. The context-category (i.e., basic vs subordinate) is added to prior set of comparison classes.
* `jointL0_wExptContext_jointStatePrior_wL1.wppl`:
  * the representation of the set-up is supposed to match the experimental set-up: a target of a known subordinate category appears in either a basic-level or a subordinate context
  * joint L0 evaluates an utterance given a known comparison class and context (via one joint meaning function for subject && predicate)
  * *joint statePrior function* (used by L0): the referent is sampled from the union of target & context; the size from the distribution according to comparison class inferred by L1 / from predicate-N of the sentence (conceptually, it matches the model above)
  * S1 adjusts the chosen utterances according to category of the target and the intended comparison class (for predicate-N); the referential utility is computed implicitly via conditioning on samples from L0 matching the intended referent  
  * *L1 is assumed to know the subordinate category of the target referent and the context*; L1 infers the distributions over size and the comparison class. The context-category (i.e., basic vs subordinate) is added to the prior set of comparison classes (i.e. it is ['sub', 'basic', context]) since it provides a plausibly salient potential comparison class.
