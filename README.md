# kaggle_quora_dupes - journal of progress


Alright, so I really think that word embeddings are going to be what sets people apart in this competition, so that is what my focus has been on so far. I will then try to go
back and fill in all the gaps with other manually generated features.

## 5/1/7

As of 5/1/2017, I've been focusing exclusively on `train` and the pre-trained `50-dim, 400k vocab, GloVe` word vector. I'm really thinking I'll need to boost this up to the 300-dim and something million vocab version 
pretty soon. I want my pre-processing to be a bit more streamlined before adding a larger word vector to the mix, however.



#### Summary of current pre-processing steps:


1) I first calculate the tf-idf of all of the words grouped by `qid` (question id) as the "document"
2) I join the `GloVe` pre-trained vector values in for each word where there is a union match between `GloVe` words and the words in my docs
3) I then use tf-idf as a weight to reduce the many **word vectors** down to a single **document vector**
	- I'm experimenting with tf-idf, tf-idf^2, all the way up to tf-idf^9, which really magnifies the weight of the higher tf-idf words
	- I'm using the `weighted.mean()` function with my transformed tf-idf values as the weights
4) From there, I join the **document vectors** to their corresponding `qid` and compare them using `text2vec` `sim2()` function (and of course, a `map()` from `purrr`)
	- I have to compare `qid1`'s tf-idf^2 and `qid2`'s tf-idf^2, etc. for all of my various tf-idf transformation strategies
5) The result of all of these comparisons is my set of features



I'm getting some decent separation between the **is_duplicate** classes, but there are a few issues as noted in the section below.



#### Summary of current issues:


* common "question words" have a low tf_idf as they appear in almost every single question
	- this causes issues with question comparisons such as "**Why** do we cry" and "**How** do we cry"
		- clearly these questions have different semantic intentions, but the current tf-idf weight strategy is missing on these
	- **resolution idea:** I'm going to set the tf-idf value manually much higher than it is for these specific types of words, so we capture their semantic meaning
* numbers are causing some huge issues, since we're tossing those out immediately. some questions are identical except for a numeric value swapped out for a different value.
* incredibly obscure words and websites in questions that are repeated often (verbatim) except for the incredibly obscure word or website which is swapped out for a different obscure word/website
* same issue as above, but for common entities that I believe we can easily recognize with some work