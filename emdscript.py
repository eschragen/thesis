#cd C:\Users\eva_s\OneDrive\Dokumente\GitHub\emfdscore

import pandas as pd 
import numpy as np
import seaborn as sns
from matplotlib import pyplot as plt
from emfdscore.scoring import score_docs 

input = pd.read_csv('C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/corpus.csv', header=None)

num_docs = len(input)

DICT_TYPE = 'emfd'
PROB_MAP = 'all'
SCORE_METHOD = 'bow'
OUT_METRICS = 'sentiment'
OUT_CSV_PATH = 'all-sent.csv'

df = score_docs(input,DICT_TYPE,PROB_MAP,SCORE_METHOD,OUT_METRICS,num_docs)
df.to_csv(OUT_CSV_PATH, index=False)

