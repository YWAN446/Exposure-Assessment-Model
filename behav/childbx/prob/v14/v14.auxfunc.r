behaviour <- function(pl,sl,hw,bt,df,et) {
  actions <- c(pl,sl,hw,bt,df,et);
  return(actions);
}

yesno <- function(arg) {
  if(is.na(arg)) return(NA);
  if(arg=="no") return(1);
  if(arg=="yes") return(2);
  return(NA);
}

soap <- function(arg) {
  if(is.na(arg)) return(NA);
  if(arg=="w/o soap") return(1);
  if(arg=="w/soap") return(2);
  return(NA);
}

wipe <- function(arg) {
  if(is.na(arg)) return(NA);
  if(arg=="no cleaned") return(1);
  if(arg=="cleaned") return(2);
  return(NA);
}

dispose <- function(arg) {
  if(is.na(arg)) return(NA);
  if(arg=="latrine") return(1);
  if(arg=="drain") return(2);
  if(arg=="trash") return(3);
  if(arg=="left on ground") return(4);
  if(arg=="nothing") return(NA);
  return(NA);
}

food <- function(arg) {
  if(is.na(arg)) return(NA);
  if(arg=="prep at home") return(1);
  if(arg=="bought") return(2);
  if(arg=="raw produce") return(3);
  if(arg=="DK") return(4);
  return(NA);
}

cutlery <- function(arg) {
  if(is.na(arg)) return(NA);
  if(arg=="hands") return(1);
  if(arg=="cutlery") return(2);
  return(NA);
}

compartment <- function(comp.descr) {
  if(is.na(comp.descr)) return(NA);
  if(comp.descr=="unimproved/dirt") return(1);
  if(comp.descr=="improved/floor") return(2);
  if(comp.descr=="off ground") return(3);
  if(comp.descr=="SWATA") return(4);
  if(comp.descr=="drain") return(5);
  if(comp.descr=="out of view") return(NA);
  return(NA);
}

mob.score <- function(mobstr,item) {
  if(mobstr=="1") return(c(2,1,1));
  if(mobstr=="2") return(c(1,2,1));
  if(mobstr=="3") return(c(1,1,2));
  if(mobstr=="1;3") return(c(2,1,2));
  return(c(NA,NA,NA)[item]);
}

