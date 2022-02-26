import argparse

import pandas as pd
from frameAxis import FrameAxis
from gensim.models import KeyedVectors


import gensim.downloader

model = gensim.downloader.load('word2vec-google-news-300')
data = pd.read_csv('C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/content_scoring.csv')

fa = FrameAxis(mfd='emfd', w2v_model=model)
mf_scores = fa.get_fa_scores(df=data, doc_colname='tweet', tfidf=False, format="virtue_vice",save_path='C:/Users/eva_s/OneDrive/MASTER/5. Semester_THESIS/Data Analytics/DATA/results_emfd.csv')
