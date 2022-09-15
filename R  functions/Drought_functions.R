## Function to compute drought characteristics
## "data" = data variable, "threshold" = threshold variable, "dates" = date variable

Droughtchar <- function(data, threshold, dates){
  dur = array()
  def = array()
  sev = array()
  st = array()
  end = array()
  p = 0
  sel = which(data < threshold)
  int = data - threshold
  last = -999
  for(i in sel){
    if((i-1) != last){
      p = p + 1
      st[p] = as.character(dates[i])
      dur[p] = 0
      def[p] = 0
      sev[p] = 0
      end[p] = 0
    }
    last = i
    dur[p] = dur[p] + 1
    def[p] = def[p] + data[i] - threshold[i]
    sev[p] = min(int[i],sev[p])
    end[p] = as.character(dates[i])
  }
  return(as.data.frame(cbind(st, end, dur, signif(-def, digits=4), signif(-sev, digits=4))))
}
