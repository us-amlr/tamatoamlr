## code to prepare `pinniped.sp.list` dataset goes here
pinniped.sp <- c("fur seal", "crabeater seal", "elephant seal", "leopard seal", "weddell seal")
pinniped.sp.list <- as.list(pinniped.sp)
names(pinniped.sp.list) <- stringr::str_to_sentence(pinniped.sp)

usethis::use_data(pinniped.sp.list, overwrite = TRUE)
