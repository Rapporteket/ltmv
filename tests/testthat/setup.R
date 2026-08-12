# rhub/r-minimal (Alpine) har R bygd utan cairo; bruk svglite for figurar
if (!capabilities("cairo")) {
  knitr::opts_chunk$set(dev = "svglite")
}
