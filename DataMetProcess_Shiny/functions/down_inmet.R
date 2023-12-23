list_inmet <- function(ano=NULL){

  if(exists("temp")){
    invisible()
  }else{
    temp <- tempfile()
  }
  download.file(paste0(
    "https://portal.inmet.gov.br/uploads/dadoshistoricos/",
    ano,
    ".zip"
  ),temp)

  df <- utils::unzip(zipfile = temp, list = TRUE)[1]
  #df <- utils::unzip(zipfile = "./Downs/zip_inmet.zip", list = TRUE)[1]

  df2 <-
    df %>%
    separate(Name,
             c("Ano","Origem"),
             sep=c("/")
    )

  if(all(is.na(df2$Origem[4:nrow(df2)]))){
    df2$Origem <- df2$Ano
    df2$Ano <- ano
  }
  suppressWarnings(
  df2 <-
    df2 %>%
    separate(
      Origem,
      c("Origem","Região","Estado","Id","Cidade","Data.Inicial","apagar","Data.Final"),
      sep=c("_")
    ) %>%
    #na.omit() %>%
    filter(!row_number() == 1) %>%
    mutate(Data.Final = as.Date(sub(".CSV","",Data.Final),"%d-%m-%Y"),
           Data.Inicial = as.Date(Data.Inicial,"%d-%m-%Y")) %>%
    select(-apagar) %>%
    arrange(Cidade))

  return(list(df,df2,temp))
}

down_inmet <- function(
    zipfile = NULL,
    filen = NULL,
    exdir = tempdir(),
    file = NULL,
    message = T
){
  unzipf <- unzip(zipfile = zipfile,
                  files = filen,
                  exdir = exdir,
                  overwrite = TRUE)

  if(!is.null(file)){

    file.copy(unzipf,file)
    if(message){
      print(paste0("Arquivo salvo em ",file))
    }
    invisible(file.remove(unzipf))

  }else{
    if(message){
      print(paste0("Arquivo salvo em ",exdir))
    }
  }
}

