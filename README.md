# Sentiment Analysis and Social Interaction Analysis of an Online Forum

Author: Kuah Jia Chen

## Introduction

The purpose of this project is to analyze the activity, language use, and social interactions in an online community using sentiment analysis and statistical techniques. The data for analysis is obtained from a real online forum, and the programming language used for this analysis is R.

## Data

The data used in this analysis is stored in the file `webforum.csv`. It includes both the metadata and linguistic analysis of posts spanning the years 2002 to 2011. For this project, a subset of 20,000 randomly selected posts from the original file will be used.

The linguistic analysis was performed using Linguistic Inquiry and Word Count (LIWC), a tool that measures the prevalence of specific thoughts, feelings, and motivations by calculating the proportion of key words used in communication. If you're interested in learning more about LIWC, you can visit the [LIWC website](http://liwc.wpengine.com/). Additionally, the language manual, which provides detailed information about the analysis process, can be accessed directly from [here](http://liwc.wpengine.com/wp-content/uploads/2015/11/LIWC2015_LanguageManual.pdf).


## Methodology

The analysis is divided into three main components:

1. Activity Analysis: This component focuses on understanding the participants' activity levels over the long term, analyzing trends, and identifying periods of increased or decreased activity.

2. Language Use Analysis: This component involves examining the linguistic variables used by the participants and comparing the language used by different authors within the forum.

3. Social Interaction Analysis: This component explores the social interactions among the participants, including network analysis and sentiment analysis to understand the overall sentiment and social dynamics within the community.

## Activity Analysis

To assess the participants' activity over the long term, the data is preprocessed by grouping it by months and years. The frequency of posts is calculated for each group, resulting in a dataset that contains the number of posts for each month and year. Time series analysis is then performed to visualize the post activity over time, identifying any significant changes or trends.

## Language Use Analysis

The language use analysis focuses on understanding the linguistic variables used by the participants. The linguistic variables include metrics such as word count, tone, authenticity, and emotional expressions. The R programming language is utilized to calculate the average values of these linguistic variables for each author and compare them.

## Social Interaction Analysis

The social interaction analysis aims to uncover the social dynamics within the online community. Network analysis techniques are employed to identify key participants, influential authors, and communities within the forum. Sentiment analysis is also conducted to assess the overall sentiment expressed in the posts and detect any significant patterns or changes in sentiment over time.

## Results and Findings

The results of the activity analysis provide insights into the participants' engagement levels over time. The time series analysis reveals periods of increased or decreased activity, indicating potential shifts in participant interest or engagement.

The language use analysis helps understand the linguistic patterns among authors within the community. By comparing the linguistic variables of different authors, we can identify any significant differences or similarities in language use.

The social interaction analysis uncovers the social dynamics within the online forum. Network analysis identifies key participants and influential authors, shedding light on the social structure of the community. Sentiment analysis provides insights into the overall sentiment expressed in the posts, allowing us to identify any shifts in sentiment and detect influential events or discussions within the forum.

## Conclusion

This project utilizes sentiment analysis, statistical techniques, and network analysis to analyze the activity, language use, and social interactions within an online community. The findings from this analysis contribute to a better understanding of the dynamics and patterns within the forum. The R programming language serves as a powerful tool for conducting the analysis and deriving meaningful insights from the data.

For a detailed analysis and findings, please refer to the complete report.