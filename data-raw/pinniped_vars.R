pinniped.afs <- stringr::str_to_sentence("fur seal")
pinniped.phocid <- stringr::str_to_sentence(
  c("crabeater seal", "elephant seal", "leopard seal", "weddell seal")
)

## code to prepare `pinniped.sp` dataset goes here
pinniped.sp <- c(pinniped.afs, pinniped.phocid)
# pinniped.sp.list <- as.list(pinniped.sp)
names(pinniped.sp) <- pinniped.sp

usethis::use_data(pinniped.sp, overwrite = TRUE)



## code to prepare `pinniped.sp.colors` dataset goes here
pinniped.sp.colors <- scales::hue_pal()(5)
names(pinniped.sp.colors) <- names(pinniped.sp)

usethis::use_data(pinniped.sp.colors, overwrite = TRUE)


## code to prepare `pinniped.phocid.sp` dataset goes here
pinniped.phocid.sp <- pinniped.sp[pinniped.sp %in% pinniped.phocid]

usethis::use_data(pinniped.phocid.sp, overwrite = TRUE)

