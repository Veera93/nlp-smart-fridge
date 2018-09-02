# Smart Refrigerator

## INTRODUCTION

This project aims at developing a natural language understanding system that can interface with a ‘smart’ refrigerator and allow users to text queries or instructions to their fridge. If the message is a declarative sentence, the fridge agrees or disagrees according to its contents. If the message is a question, the fridge answers, according to its contents. This can be achieved by developing a First-Order Logic based natural language understanding system that can create exact semantic representations for complex linguistic input, and respond appropriately.

## Modules

The below diagram describes the various modules involved in the project and gives a brief description of what each module does.

![Flow chart](/images/flow.png)

## PARSER:

For this project, we have made use of Shift Reduce parser. This being a chat application we have assumed that the query length would be short and hence the time taken to parse will not be exponential. In the future, active/passive chart parser can be used for better performance.

To convert the input sentence to FOL we had to form our grammar which consists of a list lemmas, lexical items and phrasal rules.

We have handled avoiding listing all inflections for all words by identifying the stem of each token and tagging the word appropriately. In the current version at times we might not get the expected parsed output at the first lookup but the parser will eventually return the expected FOL representation.

![Table](/images/table.png)

## MODEL CHECKER:

The model created for this project is described below,

![Model](/images/model.png)

The SAT module takes the model and the pasered input and returns the satisfiability. We have modified the sat to deal with numerals, currently model works for two and we can extend it to satisfy any numerals.

## Response

Depending of the type of the input sentence and the output of model checker we respond to the user,

1. If the input is a true declarative sentence, we output “That is correct”.
2. If the input is a false declarative sentence, we output “That is not correct”.
3. If the input is a yes-no question, then according to the output of the model checker we output yes. or No.
4. If the input is a content question, then according to the output of the model checker. The output will consist of all the properties P that the object in the answer set has, according to the model.

## Technology Stack

1. SWI-Prolog
2. Swish.swi-prolog.org 
3. Sublime text

## Example Run

![Output](/images/output.png)
