library(ggplot2)
library(lattice)
library(plotly)

#Grafikon svih drava
drzava<-baza$Drzava
drzava
zarazeni<-baza$Zarazeni
zarazeni
izljeceni<-baza$Izljeceni
umrli<-baza$Umrli
podaci<-data.frame(drzava,zarazeni,izljeceni,umrli)
fig<-plot_ly(podaci,x=~drzava, y=~zarazeni,type='bar',name="ZaraÅ¾eni virusom")
fig<-fig%>%add_trace(y=~izljeceni,name="IzljeÄeni od virusa")
fig<-fig%>%add_trace(y=~umrli,name="Umrli od virusa")
fig<-fig%>%layout(yaxis=list(title="Broj"),barmode="group")
fig

#grafikon za hrvatsku
hrv<-baza$Drzava[6]
hrv
zarazeni<-baza$Zarazeni[6]
zarazeni
izljeceni<-baza$Izljeceni[6]
umrli<-baza$Umrli[6]
umrli
data<-data.frame(hrv,zarazeni,izljeceni,umrli)
fig<-plot_ly(data,x=~hrv, y=~zarazeni,type='bar',name="ZaraÅ¾eni virusom")
fig<-fig%>%add_trace(y=~izljeceni,name="IzljeÄeni od virusa")
fig<-fig%>%add_trace(y=~umrli,name="Umrli od virusa")
fig<-fig%>%layout(yaxis=list(title="Broj"),barmode="group")
fig

#grafikon za italiju
ita<-baza$Drzava[8]
zarazeni<-baza$Zarazeni[8]
izljeceni<-baza$Izljeceni[8]
umrli<-baza$Umrli[8]
itadata<-data.frame(ita,zarazeni,izljeceni,umrli)
fig<-plot_ly(itadata,x=~ita, y=~zarazeni,type='bar',name="ZaraÅ¾eni virusom")
fig<-fig%>%add_trace(y=~izljeceni,name="IzljeÄeni od virusa")
fig<-fig%>%add_trace(y=~umrli,name="Umrli od virusa")
fig<-fig%>%layout(yaxis=list(title="Broj"),barmode="group")
fig

#rast u hrv
library(plotly)
xform<-list(categoryorder="array",
            categoryarray=c("22/01","23/01", "24/01", "25/01", "26/01", "27/01",
            "28/01", "29/01","30/01","31/02","01/02","02/02","03/02","04/02","05/02","06/02","07/02","08/02","09/02","10/02",
            "11/02","12/02","13/02","14/02","15/02","16/02","17/02","18/02","19/02","20/02","21/02","22/02","23/02","24/02",
            "25/02","26/02","27/02","28/02","29/02","01/03","02/03","03/03","04/03","05/03","06/03","07/03","08/03","09/03",
            "10/03","11/03","12/03","13/03","14/03","15/03","16/03","17/03","18/03","19/03","20/03","21/03","22/3"))
plot_ly(
  x=c("22/01","23/01", "24/01", "25/01", "26/01", "27/01",
      "28/01", "29/01","30/01","31/02","01/02","02/02","03/02","04/02","05/02","06/02","07/02","08/02","09/02","10/02",
      "11/02","12/02","13/02","14/02","15/02","16/02","17/02","18/02","19/02","20/02","21/02","22/02","23/02","24/02",
      "25/02","26/02","27/02","28/02","29/02","01/03","02/03","03/03","04/03","05/03","06/03","07/03","08/03","09/03",
      "10/03","11/03","12/03","13/03","14/03","15/03","16/03","17/03","18/03","19/03","20/03","21/03","22/3"),
  y=cro$Value[1:61],
  name = "Rast zaraÅ¾enih u hrvatskoj",
  type="bar") %>%
  layout(xaxis=xform)

#rast u italiji
library(plotly)
xform<-list(categoryorder="array",
            categoryarray=c("23/01", "24/01", "25/01", "26/01", "27/01",
                            "28/01", "29/01","30/01","31/02","01/02","02/02","03/02","04/02","05/02","06/02","07/02","08/02","09/02","10/02",
                            "11/02","12/02","13/02","14/02","15/02","16/02","17/02","18/02","19/02","20/02","21/02","22/02","23/02","24/02",
                            "25/02","26/02","27/02","28/02","29/02","01/03","02/03","03/03","04/03","05/03","06/03","07/03","08/03","09/03",
                            "10/03","11/03","12/03","13/03","14/03","15/03","16/03","17/03","18/03","19/03","20/03","21/03","22/3"))
plot_ly(
  x=c("23/01", "24/01", "25/01", "26/01", "27/01",
      "28/01", "29/01","30/01","31/02","01/02","02/02","03/02","04/02","05/02","06/02","07/02","08/02","09/02","10/02",
      "11/02","12/02","13/02","14/02","15/02","16/02","17/02","18/02","19/02","20/02","21/02","22/02","23/02","24/02",
      "25/02","26/02","27/02","28/02","29/02","01/03","02/03","03/03","04/03","05/03","06/03","07/03","08/03","09/03",
      "10/03","11/03","12/03","13/03","14/03","15/03","16/03","17/03","18/03","19/03","20/03","21/03","22/3"),
  y=ita$Value[1:60],
  name = "Rast zaraÅ¾enih u italiji",
  type="bar") %>%
  layout(xaxis=xform)

