import twint

c = twint.Config()

searchstr = "cocacola (plastic OR pollution OR recycling OR recycled OR waste OR sustainability OR world OR water OR plasticpollution)"
#searchstr = "cocacola greenwashing"
c.Search = searchstr
c.Output = "cocacola_keywords.csv"

c.Since = "2020-11-01"
c.Until = "2021-11-01"
c.Lang = "en"

c.Store_csv = True

twint.run.Search(c)


###SHELL
#climate OR carbon OR planet OR emissions OR green OR climatecrisis
##climatecrisis OR #climateemergency OR #climatechange OR #makethefuture OR #climateaction OR #climate OR #shellmustfall OR #keepitintheground OR #shellknew OR #generationdiscover OR #shellout OR #netzero OR #shellthetruth OR #actnow OR #climatejustice OR #ecocide OR #tellthetruth OR #shellguilty OR #stopshell OR #climatecriminals
#c.Since = "2020-11-01"
#c.Until = "2021-11-01"

####VW
#scandal OR dieselgate OR emissions OR green OR climate OR lying OR sustainability 
##dieselgate OR #sustainability OR #vwgate OR #climate OR #emissions OR #fraud OR #climatechange OR #green OR #transparency OR #advertising OR #cheaters OR #climatecrisis OR #vwtf
#c.Since = "2015-09-16"
#c.Until = "2016-09-16"

###STARBUCKS
#Plastic OR recycling OR reusable OR recycled OR garbage OR claims
##plasticpollution OR #sustainability OR #recycling OR #zerowaste OR #climatecrisis OR #plastic OR #recycle OR #reduce OR #reusable OR #singleuseplastic OR #strawban OR #sustainable OR #boycottstarbucks OR #breakfreefromplastic

###NESTLE
#Plastic OR water OR palmoil OR sustainable OR deforestation OR plasticpollution OR climate
##palmoil OR #deforestation OR #plasticpollution OR #boycott OR #wildlife OR #plastic OR #boycottpalmoil OR #ecocide OR #sustainability OR #breakfreefromplastic OR #boycottnestle OR #circulareconomy OR #landgrabbing OR #pollution OR #climateaction OR #climatechange OR #climatecrisis OR #healthwashing OR #netzeroemissions OR #nestlezeronet
#c.Since = "2020-11-01"
#c.Until = "2021-11-01"

###IKEA
#Deforestation OR wood OR illegal OR actually OR reduce OR world OR sustainability OR tree
##sustainability OR #deforestation OR #climateaction OR #climatecrisis OR #environment OR #ikeagreenwashing OR #airpollution OR #avoidplastic OR #blameshifting OR #bullshit OR #carbonoffsets OR #circularbullshit OR #climate OR #climatechangeresponsibility OR #climatepositive OR #CSR OR #ikeahouseofhorrors
#c.Since = "2020-08-21"
#c.Until = "2021-08-21"

###H&M
#Sustainable OR sustainability OR claims OR eco OR transparency
##fastfashion OR #sustainability OR #sustainablefashion OR #sustainable OR #ethicalfashion OR #recycling OR #hmconscious OR #whomademyclothes OR #climatechange OR #environment OR #pollution OR #climatecrisis OR #climateaction OR #ecofashion OR #recycle OR #slowfashion OR #bethechange
#c.Since = "2020-08-23"
#c.Until = "2021-08-23"

###EXXONMOBIL
#Sustainable OR sustainability OR claims OR eco OR transparency
##fastfashion OR #sustainability OR #sustainablefashion OR #sustainable OR #ethicalfashion OR #recycling OR #hmconscious OR #whomademyclothes OR #climatechange OR #environment OR #pollution OR #climatecrisis OR #climateaction OR #ecofashion OR #recycle OR #slowfashion OR #bethechange
#c.Since = "2020-11-01"
#c.Until = "2021-11-01"

###UNILEVER
#lies OR boycottunilever OR destruction OR rainforest OR plastic OR palmoil OR sustainable OR deforestation 
##palmoil OR #deforestation OR #boycott OR #wildlife OR #boycottpalmoil OR #plastic OR #ecocide OR #plasticpollution OR #landgrabbing OR #sustainability OR #climatecriminals OR #filthypalmoil OR #plasticfree OR #plastics OR #pollution OR #breakfreefromplastic OR #environment OR #cleanupkodi
#c.Since = "2020-11-01"
#c.Until = "2021-11-01"

###COCACOLA
#plastic OR pollution OR recycling OR recycled OR waste OR sustainability OR world OR water OR plasticpollution
##plastic OR #plasticpollution OR #sustainability OR #circulareconomy OR #healthwashing OR #breakfreefromplastic OR #recycled OR #sdgs OR #worldwithoutwaste OR #recycling OR #climatechange OR #plasticwaste OR #worldwithoutplastic OR #climatecrisis OR #pollution OR #environment OR #climateaction OR #plasticfree OR #plasticpollutes OR #sustainable OR #climateemergency
#c.Since = "2020-11-01"
#c.Until = "2021-11-01"