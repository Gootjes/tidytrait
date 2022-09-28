
## Meta info
vector_to_df <- function(x, .as_numeric = TRUE) {
  
  n <- names(x)
  
  # class is preserved in another attribute
  xc <- as.character(x)
  
  tibble(names = n, values = xc, i = 1:length(x), class = class(x))
}

attributes_to_df <- function(attribs, .name = NULL) {
  lapply(attribs, function(attrib) {
    if(is.vector(attrib)) {
      df <- vector_to_df(attrib)
      if(!is.null(.name)) {
        df %>% mutate(name = .name) %>% select(name, everything())  
      } else {
        df
      }
    } else {
      stop("not supported attribute type")
    }
  })
}

cbind_attribute_dfs <- function(dfs) {
  result <- list()
  
  for(df in dfs) {
    for(n in names(df)) {
      if(is.null(result[[n]])) {
        r <- df[[n]]
      } else {
        pre_df <- result[[n]]
        r <- bind_rows(pre_df, df[[n]])
      }
      result[[n]] <- r
    }
  }
  
  result
}

df_attributes_to_db <- function(x) {
  all_attribs <- lapply(colnames(x), function(vn) attributes_to_df(attributes(x[[vn]]), .name = vn))
  
  cbind_attribute_dfs(all_attribs)
}

infuse_attributes <- function(x, attrib_db) {
  
  for(an in names(attrib_db)) {
    aset <- attrib_db[[an]]
    ns <- aset %>% pluck("name") %>% unique()
    nsr <- ns[ns %in% names(x)]
    
    for(n in nsr) {
      a <- aset %>% filter(name == !!n)
      v <- a$values
      
      if(an == "labels") {
        cl <- attrib_db$class %>% filter(name == !!n) %>% pluck("values") %>% tail(1)
        v <- as(object = v, Class = cl)
      }
      
      if("names" %in% names(a)) {
        v <- setNames(v, a$names)
      }
      
      attributes(x[[n]])[[an]] <- v
    }
    
  }
  
  as_tibble(x)
}




#'@export
#'@importFrom dplyr bind_rows
#'@importFrom dplyr `%>%`
#'@importFrom dplyr `select`
#'@importFrom dplyr `left_join`
#'@importFrom dplyr `filter`
#'@importFrom dplyr `mutate`
#'@importFrom purrr `pluck`
distill_attributes <- function(x) {
  attrib_db <- df_attributes_to_db(x)  
  if (length(attrib_db) == 0) stop("no attributes found!")
  attrib_df_step1 <- do.call(bind_rows, c(list(.id = "type"), attrib_db))
  attrib_df_step1 %>% left_join(y = {
    attrib_df_step1 %>% filter(type == "label") %>% select(name, label = values)
  }, by = "name") %>%
    select(name, label, type, names, values, index = i) -> attrib_df
}

