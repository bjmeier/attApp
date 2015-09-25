getAttMap = function(x,xA,i,serStr,attMapIn)
{
  dateStr=toString(index(x[i]))
  desc=paste(dateStr,serStr)
  y=data.frame(t(x[i]))
  y$Club=rownames(y)
  row.names(y)=NULL
  colnames(y)[1]="y"
  
  yA=data.frame(t(xA[i]))
  yA$Club=rownames(yA)
  row.names(yA)=NULL
  colnames(yA)[1]="yA"
  
  attMap=join(x=attMapIn,y=y, by = 'Club',type='inner')
  attMap=join(x=attMap  ,y=yA,by = 'Club',type='inner')
  attMap$desc=desc
  

  
  ## use below for x_12, add desc. for XMMA, figure out how to set xMMA color to 'red'
  ## and to use ColorBrewer to get YoY change in color.
  
  
  
  if(serStr=="Attendance") {
  attMap$popup=paste("<strong><span style='color: MediumBlue;'>",attMap$Club,"</span></strong><br>",
                       "<span style='color: Navy;'>",attMap$League,"</span><br>",
                       attMap$desc," = ",
                       formatC(attMap$y,big.mark=",",format="f",digits=0),
                       sep="")}

  
  if(serStr=="Change in Attendance") {
    attMap$y[is.nan(attMap$y)]=0
    attMap$popup=paste("<strong><span style='color: MediumBlue;'>",attMap$Club,"</span></strong><br>",
                         "<span style='color: Navy'</span>",attMap$League,"</span><br>",
                         attMap$desc," = ",
                         formatC(attMap$yA,big.mark=",",format="f",digits=2),
                         "<br>",dateStr," Attendance =",
                         formatC(attMap$y,big.mark=",",format="f",digits=0),
                         sep="")}

  
  return(attMap)
}
