# kpia kodu z wpisu na blogu
# http://gradientdescending.com/bayesian-network-example-with-the-bnlearn-package/

library(DAAG)
library(bnlearn)
library(tidyverse)
library(visNetwork)

data('ais')
ais_sub <- ais %>% 
  select(sex, sport, pcBfat, hg, rcc, hc)


# 
# ggplot(ais_sub, aes(x = sport, y = hg, fill = sport)) + 
#   geom_boxplot() + 
#   scale_fill_manual(values = colorRampPalette(king.yna)(10))

#structure <- hc(ais_sub, score = "bic-cg")

bn_struct <- model2network("[sex][sport|sex][hg|sex][pcBfat|sex:sport][hc|hg:pcBfat][rcc|hc]")
bn_struct


bn_mod <- bn.fit(bn_struct, data = ais_sub, method = "mle")



ais_sim <- rbn(bn_mod, 2000)

ais_comb <- ais_sub %>% 
  mutate(set = 'ori') %>% 
  bind_rows(ais_sim %>% mutate(set = 'sim'))

creditmodel::get_psi(ais_comb, x = 'pcBfat', occur_time = 'set')


ggplot(ais_comb) +
  geom_density(aes(x = pcBfat, fill = as_factor(set)),
               alpha = 0.5) 


ggplot(ais_comb) +
  geom_density(aes(x = hc, fill = as_factor(set)),
               alpha = 0.5) 


ais_miss <- ais_sub
miss_id <- sample(1:nrow(ais_sub), 50)
ais_miss[miss_id, c("hg", "hc")] <- NA


ais_imp <- impute(bn_mod, data = ais_miss, method = "bayes-lw")

ais_sub %>% 
  slice(miss_id) %>% 
  mutate(set = 'orig') %>% 
  bind_rows(ais_imp %>% 
            slice(miss_id) %>% 
              mutate(set = 'uzupełnione')) %>% 
  ggplot() +
  geom_point(aes(x = hg, y = hc)) +
  facet_grid(cols = vars(set))

plot.network <- function(structure, ht = "400px"){
  nodes.uniq <- unique(c(structure$arcs[,1], structure$arcs[,2]))
  nodes <- data.frame(id = nodes.uniq,
                      label = nodes.uniq,
                      color = "darkturquoise",
                      shadow = TRUE)
  
  edges <- data.frame(from = structure$arcs[,1],
                      to = structure$arcs[,2],
                      arrows = "to",
                      smooth = TRUE,
                      shadow = TRUE,
                      color = "black")
  
  return(visNetwork(nodes, edges, height = ht, width = "100%"))
}

df <- ais_sub %>% 
  mutate(type = "orig") %>% 
  bind_rows(
    rbn(bn_mod, 2002) %>% 
      mutate(type = "sim")
  )

gg_list <- list()
grp_var <- "type"
vars <- colnames(df)[colnames(df) != grp_var]
for(k in 1:length(vars)){
  var_k <- vars[k]
  gg_list[[k]] <- ggplot(df, aes_string(x = var_k, fill = grp_var, col = grp_var))
  if(is.numeric(df[[var_k]])){
    gg_list[[k]] <- gg_list[[k]] + geom_density(alpha = 0.85, size = 0)
  }else{
    gg_list[[k]] <- gg_list[[k]] + geom_bar(position = "dodge")
  }
  gg_list[[k]] <- gg_list[[k]] + 
    theme(
      axis.text.x = element_text(angle = 90),
      axis.title.x = element_blank()
    ) +
    labs(title = var_k)
}

