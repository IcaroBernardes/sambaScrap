# 0. Setup inicial ###########
## Carrega bibliotecas
library(httr2)
library(rvest)
library(xml2)
library(purrr)
library(glue)
library(dplyr)
library(tidyr)
library(stringr)
library(pbapply)
library(future)

# 1. Extração da lista de músicas ###########
## Lista os artistas da categoria "Samba"
artLista <- rvest::read_html("https://www.vagalume.com.br/browse/style/samba.html") |> 
  rvest::html_elements(".namesColumn a")

## Obtém nomes e links para as páginas dos artistas
artistas <- dplyr::tibble(
  url_artista = rvest::html_attr(artLista, "href"),
  nome_artista = rvest::html_text2(artLista)
) |> 
  dplyr::mutate(url_artista = glue::glue("https://www.vagalume.com.br{url_artista}"))

## Cria uma função para coletar as músicas de cada artista
discografia <- function(url) {
  
  ### Lista as músicas do artista
  musLista = url |> 
    rvest::read_html() |> 
    rvest::html_elements("#alfabetMusicList a")
  
  ### Obtém links para as páginas das músicas
  musLinks = musLista |> 
    rvest::html_attr("href")
  
  ### Obtém nomes das músicas
  musNomes = musLista |> 
    rvest::html_text()
  
  ## Adequa os dados
  musica = dplyr::tibble(
    url_musica = musLinks,
    nome_musica = musNomes
  ) |> 
    dplyr::filter(stringr::str_detect(url_musica, "#play$", negate = TRUE)) |> 
    dplyr::mutate(
      url_musica = glue::glue("https://www.vagalume.com.br{url_musica}")
    )
  
  ## Aguarda por 1s (backoff) e retorna resultado
  Sys.sleep(1)
  return(musica)
  
}

## Modifica a função adicionando alternativa a falhas
discografia <- purrr::possibly(discografia)

## Efeuta a extração paralelizada
musLista <- pbapply::pblapply(
  artistas$url_artista, discografia, cl = future::availableCores()-1
)

## Une os bancos
musicas <- artistas |> 
  dplyr::mutate(data = musLista) |> 
  tidyr::unnest(cols = data)

# 2. Extração dos dados das músicas ###########
## Cria uma função para coletar os dados de cada música
album <- function(url) {
  
  ### Lê a página da música
  pagina = url |> 
    rvest::read_html()
  
  ### Obtém a letra
  letra = pagina |> 
    rvest::html_element("#lyrics") |> 
    rvest::html_text2()
  
  ### Obtém as tags de estilo
  estilo = pagina |> 
    rvest::html_elements(".h14 a") |> 
    rvest::html_text()
  
  ## Une os dados
  letra = dplyr::tibble(
    letra_musica = letra,
    estilo_musica = list(estilo)
  )
  
  ## Aguarda por 0.5s (backoff) e retorna resultado
  Sys.sleep(0.5)
  return(letra)
  
}

## Modifica a função adicionando alternativa a falhas
album <- purrr::possibly(album)

## Efeuta a extração paralelizada
letLista <- pbapply::pblapply(
  musicas$url_musica, album, cl = future::availableCores()-1
)

## Une os bancos
letras <- musicas |> 
  dplyr::mutate(data = letLista) |> 
  tidyr::unnest(cols = data)

## Salva todos os resultados
saveRDS(letras, "letras.RDS")
