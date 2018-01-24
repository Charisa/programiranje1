########################################################################
# UVOZ KNJIZNIC
########################################################################

import pandas as pd
from bs4 import BeautifulSoup
import os.path
import pridobivanje_podatkov
import numpy as np


########################################################################
# POTREBNI PODATKI ZA ANALIZO
########################################################################

seznam_kriptovalut = np.array(pridobivanje_podatkov.list_cryptos)
index = np.where(seznam_kriptovalut == "HempCoin")
seznam_kriptovalut = np.delete(seznam_kriptovalut, index, 0)
seznam_kriptovalut[777] = "Money"
df = pd.read_csv("../podatki/kriptovalute_max_volume.csv", parse_dates=["Date"])


########################################################################
# FUNKCIJE
########################################################################

def naredi_podsezname(kriptovalute, df, stolpec):
    '''Funkcija vzame podatke iz dataframe-a
    in jih shrani v podsezname novega seznama. '''
    s = []
    for i in kriptovalute:
        kripto = df[df["Name"] == i]
        vrednosti = np.array(kripto[stolpec].tolist())
        s.append(np.array(vrednosti))
    seznam = np.array(s)
    return seznam

def sestej_po_datumu(ime_stolpca, data):
    '''FUukcija vzame vse kriptovalute in sesteje vrednosti v stolpcu
    na iste dneve.'''
    data[ime_stolpca] = pd.to_numeric(df[ime_stolpca], errors="coerce")
    vsota = data.groupby("Date")[ime_stolpca].sum()
    return vsota

def izracun_glajenih_vrednosti(podatki, vrsta, k = 5,inverse_datumi = True):
    '''Izracun glajenih vrednosti - z drsecim povprecjem in eksponentnim glajenjem.
    Funkcija sprejme k in podatke, torej cene kriptovalut v letu 2017,
    in vrne seznam glajenih vrednosti. Funkcija sprejme tudi parameter k, ki v primeru
    racunanja z drsecim povprecjem predstavlja red, v primeru eksponentnega glajenja
    parameter alpha.'''
    glajene_vrednosti = []
    if vrsta == "drsece_povprecje":
        if inverse_datumi:
            for podatek in podatki:
                for i in range(len(podatek)):
                    if i > len(podatek) - k:
                        y = np.nan
                    else:
                        y = sum(podatek[i:i+k])/k
                    glajene_vrednosti.append(y)
            sez = np.array(glajene_vrednosti)
        else:
            for podatek in podatki:
                for i in range(len(podatek)):
                    if i <  k -1:
                        y = np.nan
                    else:
                        y = sum(podatek[i:i+k])/k
                    glajene_vrednosti.append(y)
            sez = np.array(glajene_vrednosti)
    elif vrsta == "eksponentno_glajenje":
        if inverse_datumi:
            j = 0
            for podatek in podatki:
                if len(podatek) >= 1:
                    glajene_vrednosti.append(podatek[0])
                    for i in range(1,len(podatek)):
                        y = k*podatek[i] + (1-k)*glajene_vrednosti[j-1]
                        glajene_vrednosti.append(y)
                        j += 1
                #elif len(podatek) == 1:
                 #   glajene_vrednosti.append(podatek[0])
            sez = np.array(glajene_vrednosti)
            print(len(sez))
        else:
            z = 0
            for podatek in podatki:
                if len(podatek) > 1:
                    glajene_vrednosti.append(podatek[0])
                    for i in range(1,len(podatek)):
                        y = k * podatek[i] + (1 - k) * glajene_vrednosti[i - 1]
                        glajene_vrednosti.append(y)
                else:
                    print(podatek, z)
                    glajene_vrednosti.append(np.nan)
                z += 1
            sez = np.array(glajene_vrednosti)
    return sez


def ustvari_csv(dataframe, vrednosti_stolpca, ime_datoteke, stolpci = None, new_len = True):
    '''Funkcija sprejme dataframe in iz njega vzame samo dane stolpce
    in spremeni vrednosti zadnjemu danemu stolpcu. Nov dataframe shrani v
    novo csv datoteko.'''
    if os.path.isfile("../" + "podatki/"+ ime_datoteke):
        return
    if new_len:
        d = dataframe
        d.loc[:,"GlajeneVrednosti"] = vrednosti_stolpca
        d.to_csv("../" + "podatki/" + ime_datoteke, index=False, header= ["Volume", "GlajeneVrednosti"])
    else:
        d = dataframe.loc[:,stolpci]
        d.loc[:,"GlajeneVrednosti"] = vrednosti_stolpca
        stolpci.append("GlajeneVrednosti")
        d.to_csv("../" + "podatki/" + ime_datoteke, index = False, header = stolpci)
    return


# Naredimo seznam podseznamov high dnevnih vrednosti vseh kriptovalut
seznam_high = naredi_podsezname(seznam_kriptovalut, df, "High")



##################### MAKSIMALNE DNEVNE VREDNOSTI ######################


########################################################################
# 1. GLAJENJE CASOVNIH VRST Z DRSECIM POVPRECJEM REDA k
########################################################################

# Glajenje z drsecim povprecjem reda k casovni vrsti priredi glajene
# vrednosti, ki so povprecja zadnjih k vrednosti.
# Glajene vrednosti bomo izracunali za red 5 in 14.

drsece_povprecje = "drsece_povprecje"
#glajene_vrednosti_drsece_povprecje5 = izracun_glajenih_vrednosti(seznam_high, drsece_povprecje)
#glajene_vrednosti_drsece_povprecje14 = izracun_glajenih_vrednosti(seznam_high, drsece_povprecje, k = 14)

data = df.copy()
#nova_tabela_high5 = ustvari_csv(data,  glajene_vrednosti_drsece_povprecje5, "glajene_vrednosti_drsece_povprecje_high_5.csv", ["Date", "Name", "High"], False)
#nova_tabela_high14 = ustvari_csv(data,  glajene_vrednosti_drsece_povprecje14, "glajene_vrednosti_drsece_povprecje_high_14.csv", ["Date", "Name", "High"], False)



########################################################################
# 2. EKSPONENTNO GLAJENJE S PARAMETROM alpha
########################################################################

# Enostavno eksponentno glajenje je podano z zaccetno vrednostjo l`1 = y1
# in rekurzivno formulo l`t = αyt + (1 − α)l`t−1, kjer je 0 ≤ α ≤ 1.
# Glajene vrednosti bomo izracunali za alpha = 0.5.

eksponentno_glajenje = "eksponentno_glajenje"
alpha = 0.5
#glajene_vrednosti_eksponentno = izracun_glajenih_vrednosti(seznam_high, eksponentno_glajenje, k = alpha)
#nova_tabela_high05 = ustvari_csv(data, glajene_vrednosti_eksponentno, "glajene_vrednosti_eksponentno_05.csv", ["Date", "Name", "High"], False)



##################### DNEVNI VOLUMEN ######################











# Izracunamo dnevne vsote celotne kolicine trgovanja na trgu
# sestej_po_datumu


#dataframe = df.copy()
#seznam_volume = sestej_po_datumu("Volume", dataframe)
#seznam_sum_volume = seznam_volume.tolist()
#d = df["Date"]
#datumi = d[:365].tolist()
#print(seznam_volume)

#glajene_vrednosti_vsota_volumen = izracun_glajenih_vrednosti([seznam_sum_volume], inverse_datumi = False)

#nova_tabela_sum_volume = ustvari_csv(seznam_volume, glajene_vrednosti_vsota_volumen, "glajene_vrednosti_drsece_povprecje_sum_volume.csv")





