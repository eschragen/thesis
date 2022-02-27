#cd C:\Users\eva_s\OneDrive\Dokumente\GitHub\twint\outputs\profile_tweets


import twint

c = twint.Config()

c.Username = "IKEA"
c.Output = "profile_ikea.csv"
c.Lang = "en"
c.Since = "2019-11-01"
c.Until = "2020-09-01"
c.Store_csv = True

twint.run.Search(c)

#starbucks @Starbucks
#nestle @Nestle
#mcdonalds @McDonalds
#ikea @IKEA nur bis 2020-09-24!!
#hm @hm
#exxonmobil @exxonmobil
#cocacola @CocaCola
#shell @Shell
#unilever @Unilever
#vw @VW
