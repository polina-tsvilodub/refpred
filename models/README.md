### General features of the models:
* The set of possible utterances is restricted to: "That dog is {big,small}", "That SUB_N is {big,small}", "That's a {big, small} dog", "That's a {big, small} SUB_N"
* The subject is assumed to (rather) contribute to reference, the predicate to predication.
* The representation of adjective meaning relies on threshold semantics

### Models
* `jointL0_utilityMixture_MH.wppl`:
  * speaker utility is represented as a _weighted mixture_ of _referential and predicative utility_
  * the comparison class is sampled by L1 and passed down, as in Tessler et al. (2017)
  * the model runs, but the qualitative predictions are incorrect
* `jointL0-utilityMixture-L2.wppl`:
  * an extended version of the `jointL0_utilityMixture_MH.wppl` model with an S2 and L2 --> S2 runs, but L2 times out
* `speaker-CC-choice_thesis.wppl`:
  * the _speaker_ choices a comparison class that maximizes the joint referential and predicative utility
  * the listener reasons about the comparison class the speaker chose
  * the model makes expected qualitative predictions
* `speaker_CC-choice_ref-size.wppl`:
  * identical to `speaker-CC-choice_thesis.wppl`, but the L1 uses a different state prior which includes the knowledge of the subordinate category of the target  
