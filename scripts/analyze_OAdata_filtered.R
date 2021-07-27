
# Remove records with fewer than 5 citations 
datum_filtered <- datum %>% filter(citations > 4)

jour_sum <- datum_filtered %>% group_by(jour) %>% summarise(countart=length(citations))
