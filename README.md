# manager
Tools for wrangling, understanding, and managing data.

- `loch_missingness_monster()`: Provides an easy-to-interpret breakdown of missingness in datasets | &#402;(x)
- `dup_detect()`: Identifies duplicated values in vectors/columns (beyond what `base::duplicated()` offers) and assists in removing them | &#402;(x)
- `golem()`: Retrieves geolocation data for IP addresses | &#402;(x)
- `winograd()`: Fetches a Winograd schema (from [here](https://cs.nyu.edu/~davise/papers/WinogradSchemas/WSCollection.html)) for use in survey bot detection--or for anything else (more details below) | &#402;(x)
- `non_person_regex`: A regex pattern for identifying names that are likely to be businesses, educational institutions, government entities, etc., as opposed to individuals | &#8500;
    - The package also contains a data set and accompanying function for testing the pattern's efficacy:
        - `test_names` | &#8500;
        - `check_non_person_regex()` | &#402;(x)
- `to_camel()` and `to_snake()`: Convert strings between `snake_case` and `camelCase` | &#402;(x)
- `%+%`: String-concatenation inflix operator, a la `+` in Python | &#9874;

Tags:

- &#402;(x) - function
- &#8500; - object
- &#9874; - operator

<details><summary>Click here for additional details on winograd function</summary><br/>
Each time the function is run, it pulls, via web scraping with rvest, the text of one Winograd schema from the following website (website created by Ernest Davis; available under a CC 4.0 license): https://cs.nyu.edu/~davise/papers/WinogradSchemas/WSCollection.html<br><br>

A Winograd schema is a sentence that includes an ambiguous pronoun that could refer to either of two antecedent nouns. Which noun the pronoun is rightly associated with depends on which of two words/phrases is present elsewhere in the sentence. For example:

*I spread the cloth on the table in order to [protect/display] it.*

If the sentence is written as "...to protect it," then *it* refers to the table. If the sentence is written as "...to display it," then *it* refers to the cloth.

Winograd schemas require commonsense human reasoning, and they're difficult for computers to resolve. Picking a sentence construction (e.g., "...to protect it" or "...to display it") and asking a question that tests one's understanding of the pronoun's identity (e.g., "What is being [protected][displayed]?") can be an effective way to distinguish people and bots in online surveys. (This is especially true if multiple Winograd schemas are presented; the odds of a bot successfully "guessing" its way past three Winograds is just 12.5%.)

I've implemented Winograd schemas to try and preserve data quality when collecting responses via Prolific/Reddit/MTurk/etc. My experience is that they can actually do a bit *too good* of a job of flagging responses as potential bots: It's not hard to give the wrong response to a Winograd schema, especially if you're moving quickly, but I often prefer to be overly conservative in the face of bot risk/low-attention responses.
</details>
