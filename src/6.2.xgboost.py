
import numpy as np
import pandas as pd

from xgboost import XGBClassifier
from sklearn.multiclass import OneVsRestClassifier
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import label_binarize
from sklearn.preprocessing import OneHotEncoder
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import shap

import pickle

FEATURE_MAP = {
    'sp': 'Subphenotype',
    'de.age': 'Age',
    'de.genderF': 'Gender: Female',
    'de.raceB': 'Race: Black',
    'de.raceO': 'Race: Other',
    'de.ethnicityN': 'Ethnicity: Hispanic',
    'de.smokerE': 'Smoker: Former',
    'de.smokerC': 'Smoker: Current',
    'hx.icuY': 'ICU Admit',
    'hx.ckdY': 'CKD',
    'hx.aki_st1': 'AKI Stage 1',
    'hx.aki_st2': 'AKI Stage 2',
    'hx.aki_st3': 'AKI Stage 3',
    'hx.htnY': 'HTN',
    'hx.dmY': 'DM',
    'hx.cldY': 'CLD',
    'hx.cvdY': 'CVD',
    'hx.chfY': 'CHF',
    'hx.copdY': 'COPD',
    'hx.sepsisY': 'Sepsis',
    'hx.lupusY': 'Lupus',
    'pe.cr.bl': 'Baseline Cr',
    'pe.gfr.bl': 'Baseline GFR',
    'pe.cr.v0': 'Cr',
    'pe.gfr.v0': 'GFR',
    'ca.nt_probnt': 'NT-proBNP',
    'ca.tropt': 'TropT',
    'in.cysc': 'CysC',
    'in.pth': 'PTH',
    'in.po4': 'PO4',
    'in.crp': 'CRP',
    'in.fgf23': 'FGF23',
    'in.st2': 'ST2',
    'in.gal3': 'GAL3',
    'pi.ifng': 'IFNG',
    'pi.il1b': 'IL-1B',
    'pi.il2': 'IL-2',
    'pi.il6': 'IL6',
    'pi.il8': 'IL-8',
    'pi.il10': 'IL-10',
    'pi.il12p70': 'IL-12 p70',
    'pi.il13': 'IL-13',
    'pi.tnfa': 'TNF-alpha',
    'pi.tnfr1': 'TNFR1',
    'pi.tnfr2': 'TNFR2',
    'ua.il18': 'U.IL-18',
    'ua.kim1': 'U.KIM-1',
    'ua.mcp1': 'U.MCP-1',
    'ua.ykl40': 'U.YKL-40',
    'ua.ngal': 'U.NGAL',
    'ua.umod': 'U.UMOD',
    'ua.cysc': 'U.CysC',
    'ua.osm': 'U.mOsm',
    'ua.cr': 'U.Cr',
    'ua.prot': 'U.Prot',
    'ua.alb': 'U.Alb',
    'ua.acr': 'U.ACR',
    'ua.pcr': 'U.PCR',
}

df = pd.read_csv('data/xgboost.20221204.csv')
x = df.iloc[:, 1:]
x = x.rename(columns=FEATURE_MAP)
y = df.iloc[:, 0]
y_bi = label_binarize(y, classes=[1, 2, 3, 4])

fit_xgb = XGBClassifier(
    booster='gbtree',
    objective='binary:logistic',
    n_estimators=100,
    learning_rate=0.1,
    max_depth=6,
    min_child_weight=3,
    gamma=0.1,
    colsample_bytree=0.3,
    random_state=1)
fit_xgb = OneVsRestClassifier(fit_xgb)
fit_xgb.fit(x, y_bi)

for i in range(0, 4):
    print(i, i ** 2)
    explainer = shap.TreeExplainer(fit_xgb.estimators_[i])
    shap_values = explainer.shap_values(x)
    plt.figure(figsize=(4, 4))
    shap.summary_plot(shap_values, x, max_display=20, show=False, plot_size=(6, 6), sort=True, plot_type="bar")
    cmap = plt.get_cmap('RdYlBu_r')
    for fc in plt.gcf().get_children():
        for fcc in fc.get_children():
            if hasattr(fcc, 'set_cmap'):
                fcc.set_cmap(cmap)
    plt.tight_layout()
    plt.xlim(0, 1)
    plt.xlabel('SHAP value')
    plt.savefig('fig/shap.hclust.feature_importance.%s.pdf' %i)
    plt.savefig('fig/shap.hclust.feature_importance.%s.png' %i, dpi=600)
    plt.close()


for i in range(0, 4):
    print(i, i ** 2)
    explainer = shap.TreeExplainer(fit_xgb.estimators_[i])
    shap_values = explainer.shap_values(x)
    plt.figure(figsize=(4, 4))
    shap.summary_plot(shap_values, x, max_display=20, show=False, plot_size=(6, 6), sort=True)
    cmap = plt.get_cmap('RdYlBu_r')
    for fc in plt.gcf().get_children():
        for fcc in fc.get_children():
            if hasattr(fcc, 'set_cmap'):
                fcc.set_cmap(cmap)
    plt.tight_layout()
    plt.xlim(-2, 2)
    plt.xlabel('SHAP value')
    plt.savefig('fig/shap.hclust.summary.%s.pdf' %i)
    plt.savefig('fig/shap.hclust.summary.%s.png' %i, dpi=600)
    plt.close()


# save
pickle.dump(fit_xgb, open('data/xgboost.pkl', 'wb'))

# load
fit_xgb = pickle.load(open('data/xgboost.pkl', 'rb'))









