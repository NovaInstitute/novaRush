# Survey class that is defined as a subclass of ResearchObject


# Twee dele: SurveyProcedure, describing the survey structure, and a SurveyDataSet, containing collected answers.



# Vraelys -----------------------------------------------------------------


# https://w3id.org/survey-ontology#SurveyProcedure

#  SurveyProcedure is connected to all the SurveyElements

# Datastel ------------------------------------------------------------

# CompletedSurvey is the Artifact generated as result of a SurveyCompletionTask (so elke submissie is 'n CompletedSurvey)
## Completed Survey
## IRI: https://w3id.org/survey-ontology#CompletedSurvey
## A survey completed by a participant following a survey procedure.

# CompletedSurvey is a set of CompletedQuestions representing the answers
## Completed Question
## IRI: https://w3id.org/survey-ontology#CompletedQuestion
## Answer provided by a participant in completing a survey for a specific question.




# https://w3id.org/survey-ontology#SurveyDataSet

# Set of data collected for a survey considering the completions performed by participants.

# Procedure describing the structure of the survey as an ordered graph of connected survey elements.


# https://w3id.org/survey-ontology#CompletedQuestion


# https://w3id.org/survey-ontology#completesQuestion
# Property linking a completed question to the question element of the survey that is answered by the participant.

# Metadata ------------------------------------------------------------

# Survey Completion Taskcback to ToC or Class ToC
# IRI: https://w3id.org/survey-ontology#SurveyCompletionTask

# Execution of a survey procedure by a participant. (so alle metadata gaan oor hierdie gebeurtenis)

# startedAtTime and endedAtTime

# can be associated with a SurveyTarget instance     (is dit die gerealiseerde steekproef?)


# SHACL ------------------------------------------------------------
# https://cefriel.github.io/survey-ontology/ontology/sur_shapes.ttl




