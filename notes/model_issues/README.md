# Model Issues:


Alright, so good stuff so far, I'm reasonably happy with how the word embeddings are going. There are some key issues that will need to be addressed however.


## Issue_001: Identical Questions with Common Entities Swapped Out:


I have a feeling these were the "fake" questions that quora decided to generate in order to test our ability to do some entity recognition and correctly classify extremely similar questions as a non-duplicate because of a single entity mis-match.
It is important to remove symbols before doing any tf-idf calculations. I have a hunch that we are missing a few important words in our tf-idf calculations because they have a question mark attached to them at the end of the question.


#### Specific example:

* qid 46295: "What are the main imports and exports of Venezuela, and how does Venezuela's industry compare to **Chile's?**"
* qid 7253: "What are the main imports and exports of Venezuela, and how does Venezuela's industry compare to **Canada's?**"
* This pair of questions above will score incredibly high with most raw tf-idf / word vector strategies


#### Investigation:


What I'm going to do is go back and look at what the tf-idf calculations looked like for each of these questions. That information should be held in `pre_proc_tf-idf2.R` script (keep in mind that right now this only has train data being processed in it).


I am happy to see that `canada's` and `venezuela's` are **both at the top for tf_idf value**. I think the issue is the "'s" at the end of each of these words. Whenever I do a quick `%in% names(glove)`, it comes up with a FALSE FALSE for `canada's` and `venezuela's` 
respectively. This is not surprising. This appears to be a matter of word-stemming. 


**Note:** both `canada` and `canadas` show up in the glove pre-trained vector and there is a fairly significant amount of distance between them. I assume that the only difference is that one shows a possessive quality and the other does not. 
I will need to test this for other possessive type word combinations. For instance, does `canadas - canada + venezuela = venezuelas`? 


But then when I do a quick `SnowballC::wordStem()` on these values, it retains the `'` at the end... that seems like odd behavior. 


#### Conclusion: 

* I need to experiment with word stemming
* There needs to be a deep dive on the words that are found in the questions but not in glove (some really shouldn't be included, but others are just mistakenly taken out)
* Then there needs to be a separate strategy in place for the entities that are uncommon and aren't just a dumb mistake for why the didn't show up in glove