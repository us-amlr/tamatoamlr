## code to prepare `pinniped.sp.list` dataset goes here
pinniped.sp <- c("fur seal", "crabeater seal", "elephant seal", "leopard seal", "weddell seal")
pinniped.sp.list <- as.list(pinniped.sp)
names(pinniped.sp.list) <- stringr::str_to_sentence(pinniped.sp)

usethis::use_data(pinniped.sp.list, overwrite = TRUE)



## code to prepare `pinniped.sp.colors` dataset goes here
pinniped.sp.colors <- scales::hue_pal()(5)
names(pinniped.sp.colors) <- stringr::str_to_sentence(pinniped.sp)

usethis::use_data(pinniped.sp.colors, overwrite = TRUE)
